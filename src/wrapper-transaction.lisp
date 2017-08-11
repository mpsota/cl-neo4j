;; Wrapper for transaction

(in-package #:cl-neo4j)

;; Transactions
(defclass transaction ()
  ((id
    :initarg :id
    :accessor transaction-id)
   (expires
    :initarg :expires
    :accessor transaction-expires)))

(defmethod print-object ((object transaction) stream)
  (print-unreadable-object (object stream :type t :identity '())
    (format stream "~D [EXPIRES ~A]"
            (transaction-id object)
            (transaction-expires object))))

(defvar *transaction*)

(defun begin-transaction ()
  "Begins new transaction and returns object of class `TRANSACTION'"

  (let* ((response (cl-neo4j:transaction-begin))
         (errors (cdr (assoc :errors response)))
         (commit (cdr (assoc :commit response)))
         (transaction (cdr (assoc :transaction response)))
         (expires (when transaction (cdr (assoc :expires transaction))))
         (transaction-id (when (and commit (stringp commit))
                           (cl-ppcre:register-groups-bind (tid)
                               (".*transaction\/(\\d+)\/commit" commit)
                             tid)))
         (transaction-id-numeric (when (stringp transaction-id)
                                   (parse-integer transaction-id))))
    (when errors
      (error "error while beginning transaction: ~a" errors))
    (if transaction-id-numeric
        (make-instance 'transaction
                       :id transaction-id-numeric
                       :expires expires)
        (error "error while beginning transaction: no transaction ID received: ~A" response))))

(defmethod commit-transaction ((tr transaction))
  "Commit open transaction `TRANSACTION'"
  (let* ((response (cl-neo4j:transaction-commit :transaction (transaction-id tr)))
         (errors (cdr (assoc :errors response))))
    (if errors
        (error "error while commiting transaction: ~a" errors)
        t)))

(defmethod rollback-transaction ((tr transaction))
  "Rollback open transaction `TRANSACTION'"
  (let* ((response (cl-neo4j:transaction-rollback :transaction (transaction-id tr)))
         (errors (cdr (assoc :errors response))))
    (if errors
        (error "error while rolling-back transaction: ~a" errors)
        t)))

(defmacro with-transaction (&body body)
  `(let ((*transaction* (begin-transaction)))
     ;; execute body
     (handler-case
         (prog1
             (progn ,@body)
           ;; if no errors - commit
           (commit-transaction *transaction*))
       (error (c)
         (handler-case
             (rollback-transaction *transaction*)
           (error (d)
             (format *debug-io* "Error while rolling back trasnaction: ~A" d)))
         (error c)))))

;; General queries

(defun query (query &optional include-stats)
  (handle-neo4j-query-error
   (if (boundp '*transaction*)
       (with-slots (id) *transaction*
         (prog1
             (cl-neo4j:cypher-query-in-transaction :statements
                                                   (list (make-instance 'cypher-query
                                                                        :statement query
                                                                        :include-stats include-stats))
                                                   :transaction id)
           (cl-neo4j:transaction-keep-alive :transaction id)))
       (cl-neo4j:cypher-query :statements
                              (list (make-instance 'cypher-query
                                                   :statement query
                                                   :include-stats include-stats))))
   :query (list :query query)))

(defun query-statement (statement &optional include-stats)
  (let ((query (cdr (assoc :query statement)))
        (params (cdr (assoc :params statement))))
    (handle-neo4j-query-error
     (when query
       (if (boundp '*transaction*)
           (with-slots (id) *transaction*
             (prog1
                 (cl-neo4j:cypher-query-in-transaction :statements
                                                       (list (make-instance 'cypher-query
                                                                            :statement query
                                                                            :parameters params
                                                                            :include-stats include-stats))
                                                       :transaction id)
               (cl-neo4j:transaction-keep-alive :transaction id)))
           (cl-neo4j:cypher-query :statements
                                  (list (make-instance 'cypher-query
                                                       :statement query
                                                       :parameters params
                                                       :include-stats include-stats)))))
     :query (list :query query :params params))))
