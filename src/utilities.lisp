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
  (etypecase json
    ((vector (unsigned-byte 8)) (decode-json-from-string (babel:octets-to-string json)))
    (string (decode-json-from-string json))))

(defun urlencode (string)
  (cl-ppcre:regex-replace "\\+" (drakma::url-encode string :latin1) "%20"))

(defun geta (item alist &key (test #'eq))
  "get value of item from association list"
  (cdr (assoc item alist :test test)))

(defun car* (list)
  "Like car, but assert that there is no cdr"
  (assert (listp list))
  (assert (null (cdr list))
          nil
          "~S is expected to be one element long" list)
  (car list))

;; Error handling

(define-condition neo4j-error (error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)
   (problematic-query :initarg :problematic-query :reader problematic-query))
  (:report (lambda (condition stream)
             (with-slots (code message problematic-query) condition
               (format stream "ERROR: ~A~%~A~%~%~S"
                       code
                       message
                       problematic-query)))))

(define-condition neo4j-error-retry (neo4j-error) ()
  (:report (lambda (condition stream)
             (with-slots (code message problematic-query) condition
               (format stream "RETRY: ~A~%~A~%~%~S"
                       code
                       message
                       problematic-query)))))

(defun make-neo4j-condition (&key code message problematic-query)
  (assert (stringp code))
  (let ((condition-type
          (alexandria:switch (code :test #'string=)
            ("Neo.TransientError.Transaction.DeadlockDetected" 'neo4j-error-retry)
            (t 'neo4j-error))))
    (make-condition condition-type
                    :code code
                    :message message
                    :problematic-query problematic-query)))

(defun handle-neo4j-query-error (retval &key (query nil))
  (let* ((errors (cdr (assoc :errors retval))))
    (if errors
        (let ((code (geta :code (car errors)))
              (message (geta :message (car errors))))
          (error (make-neo4j-condition :code code
                                       :message message
                                       :problematic-query query)))
        retval)))

(defmacro make-statement (query params)
  (let ((params-bindings (mapcar (lambda (param)
                                   `(cons ,(format nil "~A" param) ,param))
                                 params)))
    `(let* ((statement (list (cons :query ,query)
                             (cons :params (list ,@params-bindings)))))
       statement)))

(defun neo4j-response-to-hashtable (retval &optional extract-keys apply-fun)
  (let* ((response (handle-neo4j-query-error retval))
         (results (cadr (assoc :results response)))
         (columns (cdr (assoc :columns results)))
         (data (cdr (assoc :data results)))
         (rows (mapcar (lambda (x)
                         (cdr (assoc :row x)))
                       data))
         ;; prepare hashatble
         (retval1 (mapcar (lambda (row)
                           (loop
                              with ht = (make-hash-table :test 'equal)
                              for key in columns
                              for value in row
                              do
                                (setf (gethash key ht) value)
                              finally
                                (return ht)))
                         rows))
         ;; extract keys if requested
         (retval2 (if extract-keys
                      (mapcar (lambda (ht)
                                (mapcar (lambda (key)
                                          (gethash key ht))
                                        extract-keys))
                              retval1)
                      retval1))
         ;; apply fun if requested
         (retval3 (if apply-fun
                      (funcall apply-fun retval2)
                      retval2)))
    retval3))
