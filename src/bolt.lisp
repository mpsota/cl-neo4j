(in-package #:cl-neo4j)

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
    :record #x61
    :ignored #x7e
    :failure #x7f))

(defun response (code)
  (car (find code (loop
                    for r on +responses+ by #'cddr
                    collect r)
             :key #'cadr)))

;; Utilities

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
  (coerce data '(simple-array (unsigned-byte 8))))

(defparameter +bolt+ #(#x60 #x60 #xb0 #x17))
(defparameter +bolt-version+ 1)

;; Connection setup

(defun bolt-connect (host &optional (port 7687))
  (setf *socket* (usocket:socket-connect host port :element-type 'octet)))

(defun bolt-handshake (socket &key (version +bolt-version+))
  (let ((raw-bolt-version (reduce #'aappend (mapcar (lambda (x) (rpack x :uint32)) (list version 0 0 0)))))
    (socket-sendall socket (to-usp8-array +bolt+))
    (socket-sendall socket raw-bolt-version))
  (let* ((response (socket-recv socket 4))
         (server-version (unpack-number :uint32 response)))
    ;; TODO close socket if error
    (assert (= version server-version))
    (list :bolt version)))

;; Message chunks


