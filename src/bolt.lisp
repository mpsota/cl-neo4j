(in-package #:cl-neo4j)


(defvar *socket* nil)
(defvar *transaction* nil)

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

(defparameter *connection-pool-size* 64)

(defvar *connection-details* '(:host nil
                               :port nil
                               :login nil
                               :password nil))


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
  (usocket:socket-connect host port :element-type '(unsigned-byte 8)))

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

(defun login (socket login password)
  (bolt-message socket
                :init
                "MyClient/1.0"
                (make-map `(("scheme" "basic")
                            ("principal" ,login)
                            ("credentials" ,password)))))


;; Connection pooling

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun locked-value-lock-name (name)
    "Used for locked-values"
    (intern (format nil "~A-LOCK" name) (find-package 'curl)))

  (defmacro defvar-locked (name &optional default docstring)
    "Replacement for defvar for thread-locked variables"
    `(progn
       (defvar ,name ,default ,@(when docstring `(,docstring)))
       (defvar ,(locked-value-lock-name name) (bt:make-lock ,(string name)))))

  (defmacro with-locked-var (place &body body)
    `(bt:with-lock-held (,(locked-value-lock-name place))
       ,@body))

  (defvar-locked *connections* nil))

(defun init-connection ()
  (let ((socket (bolt-connect (getf *connection-details* :host)
                              (getf *connection-details* :port))))
    (bolt-handshake socket)
    (login socket
           (getf *connection-details* :login)
           (getf *connection-details* :password))
    (with-locked-var *connections*
      (push socket *connections*))))

(defun init-connections (&key (host "127.0.0.1") (port 7687) (login "neo4j") password (pool-size *connection-pool-size*))
  (setf *connection-details* (list :host host
                                   :port port
                                   :login login
                                   :password password))
  (dotimes (i (- pool-size (length *connections*)))
    (init-connection)))

(defun get-connection ()
  (loop
    with connection = nil
    while (null connection)
    do (setf connection
             (with-locked-var *connections*
               (pop *connections*)))
    finally (return connection)))

(defun return-connection (connection)
  (assert connection)
  (with-locked-var *connections*
    (push connection *connections*)))

(defun close-connection (connection)
  (assert connection)
  (usocket:socket-close connection))

(defmacro with-connection (() &body body)
  `(if *socket*
       ;; In case of nested execution, don't take another socket but reuse
       (progn ,@body)
       ;; Else take new one
       (let ((*socket* (get-connection)))
         (handler-case
             (prog1
                 (progn ,@body)
               ;; Return connection to pool
               (return-connection *socket*))
           (serious-condition (c)
             (close-connection *socket*)
             ;; Start new connection to replace dead one
             (init-connection)
             ;; Throw error
             (error c))))))


;; Higher-level interface
(defun bolt-query (statement statement-params)
  (assert *socket*)
  (run *socket* statement statement-params)
  (pull-all *socket*))

;;(init-connections :host "127.0.0.1" :port 7687 :login "neo4j" :password "endsec")


;; Transactions

(defun begin-transaction ()
  (assert *transaction*)
  (bolt-query "BEGIN" nil))

(defun rollback-transaction ()
  (assert *transaction*)
  (handler-case (bolt-query "ROLLBACK" nil)
    (serious-condition (c) (warn "Transaction ~S rollback failed: ~S" *transaction* c))))

(defun commit-transaction ()
  (assert *transaction*)
  (bolt-query "COMMIT" nil))

(defmacro with-transaction (&body body)
  "Execute `BODY' within one transaction. This means using only one `*SOCKET*' from pool."
  `(if *transaction*
       ;; If already in transaction, just execute
       (progn ,@body)
       ;; Else start new one
       (let* ((*transaction* (get-connection))
              (*socket* *transaction*))
         (handler-case
             (prog2
                 (begin-transaction)
                 (progn ,@body)
               (commit-transaction)
               ;; Return connection to pool
               (return-connection *socket*))
           (serious-condition (c)
             ;; rollback
             (rollback-transaction)
             ;; close connection
             (close-connection *socket*)
             ;; Start new connection to replace dead one
             (init-connection)
             ;; Throw error
             (error c))))))
