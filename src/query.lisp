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
          `(("parameters" ,@(alexandria:alist-hash-table parameters))))))) ;; without transformation to hashtable it will be encoded as array in case there is nil cdr '(("foo" . nil))

(defmethod encode-cypher-query ((q cypher-query))
  (json:encode-json-alist-to-string (structure-cypher-query q)))

(defun make-query (statement &key parameters include-stats)
  (let ((q (make-instance 'cypher-query
                          :statement statement
                          :parameters parameters
                          :include-stats include-stats
                          )))
    q))

#+test ;; expected json object, not list in both cases
(encode-neo4j-json-payload (list (make-instance 'cypher-query
                                                          :statement "CREATE (node:foo  {`foo`: {NODE_FOO}}) RETURN (node)"
                                                          :parameters '(("NODE_FOO"))
                                                          :include-stats t))
                           :statements)

#+test
(encode-neo4j-json-payload (list (make-instance 'cypher-query
                                                          :statement "CREATE (node:foo  {`foo`: {NODE_FOO}}) RETURN (node)"
                                                          :parameters '(("NODE_FOO" . 5))
                                                          :include-stats t))
                                     :statements)
