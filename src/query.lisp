(in-package #:cl-neo4j)

(defclass cypher-query ()
  ((statement :accessor statement :initform "" :initarg :statement)
   ;; `properties' is `props' field in parameters - not sure why
   ;; author decided so? maybe because that's how it is dsecribed in
   ;; API
   (properties :accessor properties :initform nil :initarg :properties)
   ;; `parameters' has been added - `properties' will be appended to
   ;; it so it should not break backward compatibility
   (parameters :accessor parameters :initform nil :initarg :parameters)))

(defmethod structure-cypher-query ((q cypher-query))
  (with-slots (statement properties parameters) q
    ;; append `properties' to `parameters' with key `params'
    (let ((parameters (append parameters (when properties `(("props" ,@properties))))))
      `(("statement" ,@(statement q))
        ,@(when parameters
                `(("parameters" ,@parameters)))))))

(defmethod encode-cypher-query ((q cypher-query))
  (json:encode-json-alist-to-string (structure-cypher-query q)))

(defun make-query (statement &key properties parameters)
  (let ((q (make-instance 'cypher-query
                          :statement statement
                          :properties properties
                          :parameters parameters
                          )))
    q))
