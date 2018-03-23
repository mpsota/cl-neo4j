(in-package #:cl-neo4j)

;; BOLT conditions

;; FIXME only one condition now...

(define-condition bolt-error (error)
  ((code :accessor code :initarg :code)
   (message :accessor condition-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Error ~A: ~A" (code condition) (condition-message condition)))))

