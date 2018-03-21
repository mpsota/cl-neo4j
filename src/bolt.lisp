(in-package #:cl-neo4j)

;;(ql:quickload :cl-pack)

(defvar *socket* nil)

(defparameter +messages+
  '(:init #x01
    :ack-failure #x0e
    :reset #x0f
    :run #x10
    :discard-all #x2f
    :pull-all #x3f))

(defun message (key)
  (getf +messages+ key))

(defparameter +responses+
  '(:success #x70
    :record #x71
    :ignored #x7e
    :failure #x7f))

(defun response (code)
  (car (find code (loop
                    for r on +responses+ by #'cddr
                    collect r)
             :key #'cadr)))

(defparameter +structure-types+
  '(:node #x4e
    :relationship #x52
    :path #x50))

(defun structure-type (code)
  (car (find code (loop
                    for r on +structure-types+ by #'cddr
                    collect r)
             :key #'cadr)))

(defparameter +bolt-message-chunk-size+ 16)

;; Utilities

(defun geta (item alist &key (test #'equal))
  "get value of item from association list"
  (cdr (assoc item alist :test test)))

(defun make-buffer (size)
  (make-array size :element-type '(unsigned-byte 8)))

(defun slurp-usb8-stream5 (stream)
  "read stream"
  (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8) :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun socket-sendall (socket data)
  (let ((stream (usocket:socket-stream socket)))
    (write-sequence data stream)
    (force-output stream)))

(defun socket-recv (socket len)
  (usocket:wait-for-input socket)
  (let ((buf (make-buffer len)))
    (read-sequence buf (usocket:socket-stream socket))
    buf))

(defun to-usp8-array (data)
  (make-array (length data)
              :element-type '(unsigned-byte 8)
              :initial-contents data))

(defun make-map (&optional keys-and-values)
  "Make map from list of lists or list of conses"
  (cons :obj (mapcar (lambda (kv)
                       (if (listp (cdr kv))
                           (cons (car kv) (cadr kv))
                           kv))
                     keys-and-values)))

;; Connection setup

(defparameter +bolt+ #(#x60 #x60 #xb0 #x17))
(defparameter +bolt-version+ 1)

(defun bolt-connect (host &optional (port 7687))
  (setf *socket* (usocket:socket-connect host port :element-type '(unsigned-byte 8))))

(defun bolt-handshake (socket &key (version +bolt-version+))
  (let ((raw-bolt-version (reduce #'aappend (mapcar (lambda (x) (rpack x :uint32)) (list version 0 0 0)))))
    (socket-sendall socket (to-usp8-array +bolt+))
    (socket-sendall socket raw-bolt-version))
  (let* ((response (socket-recv socket 4))
         (server-version (unpack-number :uint32 response)))
    ;; TODO close socket if error
    (assert (= version server-version))
    (list :bolt version)))


;; Messaging

(defun bolt-chunk (data)
  "Bolt chunk messages into up `+BOLT-MESSAGE-CHUNK-SIZE+' chunks"
  (concatenate 'vector
               (reduce (lambda (a b) (concatenate 'vector a b))
                       (mapcar (lambda (chunk)
                                 ;; Prepend each chunk with its size
                                 (concatenate 'vector
                                              (rpack (length chunk) :uint16)
                                              chunk))
                               ;; Split `DATA' by chunk size
                               (loop
                                 for head = (subseq data 0 (min +bolt-message-chunk-size+
                                                                (length data)))
                                 do (setf data (when (> (length data)
                                                        +bolt-message-chunk-size+)
                                                 (subseq data +bolt-message-chunk-size+ (length data))))
                                 collect head
                                 while data)))
               ;; Concatenate end marker #(0 0)
               #(0 0)))

(defun bolt-send (socket data)
  (socket-sendall socket (to-usp8-array (bolt-chunk data))))

(defun bolt-recv (socket)
  "Receive raw data from `SOCKET' and concatenate chunks"
  (reduce (lambda (&optional x y) (when (and x y) (concatenate 'vector x y)))
          (loop
            for chunk-length = (unpack-number :uint16 (socket-recv socket 2))
            while (> chunk-length 0)
            collect (socket-recv socket chunk-length))))

(defun bolt-response (socket)
  "Receive (and parse) message responses from `SOCKET'"
  (destructuring-bind (code response) (unpacked (bolt-recv socket))
    (list (response code) response)))


;; Node, relationship, path handling

(defun handle-record-node (record)
  (destructuring-bind (node-id labels properties) (cdr record)
    (make-instance 'neo::standard-node
                   :id node-id
                   :properties (cdr properties)
                   :labels labels)))

(defun handle-record (record)
  "Destructure `RECORD'"
  (let ((record (caar record)))
    (ecase (structure-type (car record))
      (:node (handle-record-node record)))))

;; Message handling and consuming

(defun handle-bolt-response-status (bolt-response)
  (when bolt-response
    (destructuring-bind (response &rest rest) bolt-response
      (case response
        (:failure (error (make-condition 'bolt-error
                                         :code (geta "code" (cdar rest))
                                         :message (geta "message" (cdar rest)))))
        (:record (list :record (handle-record rest)))
        (otherwise bolt-response)))))

(defun bolt-consume (socket)
  "Consume responses from SOCKET"
  (loop
    for response = (handle-bolt-response-status (bolt-response socket))
    collect response
    while (not (find (car response) '(:success :failure :ignored)))))

(defun bolt-message (socket message-key &rest args)
  "Prepare and send message over `SOCKET'"
  (bolt-send socket
             (packed (concatenate 'vector
                                  (vector (message message-key))
                                  args)))
  (bolt-consume socket))

;; Low-level interface
(defun run (socket statement statement-params)
  (bolt-message socket :run statement (funcall #'make-map statement-params)))

(defun pull-all (socket)
  (bolt-message socket :pull-all))

(defun login (login password)
  (bolt-message *socket*
                :init
                "MyClient/1.0"
                (make-map `(("scheme" "basic")
                            ("principal" ,login)
                            ("credentials" ,password)))))

;; Higher-level interface
(defun bolt-query (socket statement statement-params)
  (run socket statement statement-params)
  (pull-all socket))

