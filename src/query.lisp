(in-package #:cl-neo4j)

(defclass cypher-query ()
  ((statement :accessor statement :initform "" :initarg :statement)
   (parameters :accessor parameters :initform nil :initarg :parameters)
   (include-stats :accessor include-stats :initform nil :initarg :include-stats)))

(defmethod structure-cypher-query ((q cypher-query))
  (with-slots (statement parameters include-stats) q
    `(("statement" ,@(statement q))
      ,@(when include-stats
          `(("includeStats" ,@include-stats)))
      ,@(when parameters
          `(("parameters" ,@parameters))))))

(defmethod encode-cypher-query ((q cypher-query))
  (json:encode-json-alist-to-string (structure-cypher-query q)))

(defun make-query (statement &key parameters include-stats)
  (let ((q (make-instance 'cypher-query
                          :statement statement
                          :parameters parameters
                          :include-stats include-stats
                          )))
    q))
