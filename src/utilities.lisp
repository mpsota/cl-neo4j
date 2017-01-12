(in-package #:cl-neo4j)

(defun format-neo4j-query (host port resource &key (db-postfix "db/data/") (protocol "http"))
  (format nil "~A://~A:~A/~A~A" protocol host port db-postfix resource))

(defgeneric encode-neo4j-json-payload (object encode-type &key)
  (:method (object encode-type &key)
    (declare (ignore encode-type))
    (encode-json-to-string object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :alist)) &key)
  (declare (ignore encode-type))
  (encode-json-alist-to-string object))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :node-url)) &key host port)
  (declare (ignore encode-type))
  (format-neo4j-query host port (format nil "node/~A" object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :node-url-single)) &key host port)
  (declare (ignore encode-type))
  (encode-neo4j-json-payload (encode-neo4j-json-payload object :node-url :host host :port port) :string))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :relationship-url)) &key host port)
  (declare (ignore encode-type))
  (format-neo4j-query host port (format nil "relationship/~A" object)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :relationship-url-single)) &key host port)
  (declare (ignore encode-type))
  (encode-neo4j-json-payload (encode-neo4j-json-payload object :relationship-url :host host :port port) :string))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :object)) &key)
  (declare (ignore encode-type))
  (encode-neo4j-json-payload (mapcar (lambda (el)
                                       (cons (caar el) (if (cadr el)
                                                           (encode-neo4j-json-payload (cdar el)
                                                                                      (cadr el))
                                                           (cdar el))))
                                     object)
                             :string))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :relationship)) &key)
  (declare (ignore encode-type))
  (destructuring-bind (to type data) object
    (encode-neo4j-json-payload (list (list (cons "to" to) :node-url)
                                     (list (cons "type" type))
                                     (list (cons "data" data)))
                               :object)))

(defmethod encode-neo4j-json-payload ((object cypher-query) (encode-type (eql :statement)) &key)
  (declare (ignore encode-type))
  (encode-cypher-query object))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :statements)) &key)
  (declare (ignore encode-type))
  (let ((table (make-hash-table :test 'equalp)))
    (setf (gethash "statements" table)
          (mapcar (lambda (statement)
                    (structure-cypher-query statement))
                  object))
    (encode-neo4j-json-payload table :table)))

(defmethod encode-neo4j-json-payload (object (encode-type (eql :raw-string)) &key)
  (declare (ignore encode-type))
  (format nil "~A" object))

(defun decode-neo4j-json-output (json)
  (decode-json-from-string (babel:octets-to-string json)))

(defun urlencode (string)
  (cl-ppcre:regex-replace "\\+" (drakma::url-encode string :latin1) "%20"))
