;; cypher-api based queries, already using cl-neo4j-wrapper:query-statement which is transaction-aware
(in-package #:cl-neo4j)

(defun string-sanitize (x) (if (stringp x)
                               (string-trim ";: \"'" x)
                               x))
(defmacro def-neo4j-query-fun (name (&key (produce-ht t)
                                          (extract-keys nil)
                                          (apply-fun nil))
                                       lambda-list &body body)
  `(defun ,name (&key ,@lambda-list)
     ,(if produce-ht
          `(neo4j-response-to-hashtable
            (progn ,@body)
            ',extract-keys
            ,apply-fun)
          `(progn ,@body))))

;; not yet implemented or not available in transactional API
(def-neo4j-query-fun create-index () (type name config)
  (list type name config) ;; just ignore
  (error "`CREATE-INDEX' not yet implemented"))
(def-neo4j-query-fun delete-index () (type name)
  (list type name) ;; just ignore
  (error "`DELETE-INDEX' not yet implemented"))
(def-neo4j-query-fun list-indexes () (type)
  (list type) ;; just ignore
  (error "`LIST-INDEXES' not yet implemented"))
(def-neo4j-query-fun add-to-index () (type name key value objectid)
  (list type name key value objectid) ;; just ignore
  (error "`ADD-TO-INDEX' not yet implemented"))
(def-neo4j-query-fun remove-from-index () (type name key value objectid)
  (list type name key value objectid) ;; just ignore
  (error "`REMOVE-FROM-INDEX' not yet implemented"))
(def-neo4j-query-fun lookup-index () (type name key value)
  (list type name key value) ;; just ignore
  (error "`LOOKUP-INDEX' not yet implemented"))
(def-neo4j-query-fun query-index () (type name query)
  (list type name query) ;; just ignore
  (error "`QUERY-INDEX' not yet implemented"))
(def-neo4j-query-fun traverse () (node-id return-type max-depth order uniqueness relationships prune-evaluator return-filter)
  (list node-id return-type max-depth order uniqueness relationships prune-evaluator return-filter)
  (error "`TRAVERSE' not yet implemented"))
(def-neo4j-query-fun get-path () (node-id to-node-id relationships max-depth algorithm)
  (list node-id to-node-id relationships max-depth algorithm)
  (error "`GET-PATH' not yet implemented"))
(def-neo4j-query-fun get-paths () (node-id to-node-id relationships max-depth algorithm)
  (list node-id to-node-id relationships max-depth algorithm)
  (error "`GET-PATHS' not yet implemented"))

;; Lower level
(def-neo4j-query-fun get-node (:apply-fun 'car) (nodeid)
  (let ((statement (make-statement "MATCH (n) WHERE id(n)={NODEID} RETURN id(n) AS id, labels(n) AS labels, properties(n) AS properties" (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun create-node (:apply-fun 'car) (properties)
  (let ((statement (make-statement "CREATE (n {PROPERTIES}) RETURN id(n) as id, properties(n) as properties" (properties))))
    (query-statement statement)))

(def-neo4j-query-fun delete-node () (nodeid)
  (let* ((statement (make-statement "MATCH (n) WHERE id(n)={NODEID} DELETE n" (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun set-node-label () (nodeid label)
  ;; label name cannot be parametrized:
  ;; http://stackoverflow.com/questions/32957497/unable-to-set-node-label-dynamically-using-the-neo4j-rest-api
  (let* ((label-name (string-trim ";: \"'" label))
         (statement (make-statement (format nil "MATCH (n) WHERE id(n)={NODEID} SET n :~A" label-name) (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun create-relationship () (nodeid1 nodeid2 type properties)
  ;; seems like relation type also can not be parametrized
  (let* ((rel-type (string-trim ";: \"'" type))
         (statement (make-statement (format nil "MATCH (n1) WHERE id(n1)={NODEID1} MATCH (n2) WHERE id(n2)={NODEID2} CREATE (n1)-[:~A {PROPERTIES}]->(n2)"
                                            rel-type)
                                    (nodeid1 nodeid2 properties))))
    (query-statement statement)))

(def-neo4j-query-fun get-node-relationships () (nodeid types direction)
  (let* ((direction (when direction
                      (format nil "~(~A~)" direction)))
         (direction (cond
                      ((string= direction "all") "-[rel]-")
                      ((string= direction "in") "<-[rel]-")
                      ((string= direction "out") "-[rel]->")
                      ((null direction) "-[rel]-")
                      (t (error "Invalid direction: `~A'" direction))))
         (types (if types
                    (format nil "AND (~{type(rel)=\"~A\" ~^OR ~})" types)
                    ""))
         (statement (make-statement (format nil "MATCH (n)~A() WHERE id(n)={NODEID} ~A RETURN id(rel) AS id, type(rel) AS type, id(startNode(rel)) AS startnodeid, id(endNode(rel)) AS endnodeid, properties(rel) AS properties" direction types) (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun get-relationship-types () ()
  (let ((statement (make-statement "MATCH ()-[r]-() RETURN DISTINCT type(r)" ())))
    (query-statement statement)))

(def-neo4j-query-fun get-relationship () (relationshipid)
  (let ((statement (make-statement "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} RETURN DISTINCT id(rel) AS id, type(rel) AS type, id(startNode(rel)) AS startnodeid, id(endNode(rel)) AS endnodeid, properties(rel) AS properties" (relationshipid))))
    (query-statement statement)))

(def-neo4j-query-fun delete-relationship () (relationshipid)
  (let ((statement (make-statement "MATCH ()-[r]-() WHERE id(r)={RELATIONSHIPID} DELETE r" (relationshipid))))
    (query-statement statement)))

(def-neo4j-query-fun get-relationship-property () (relationshipid property)
  ;; property name cannot be parametrized
  (let* ((property-name (string-sanitize property))
         (statement (make-statement (format nil "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} RETURN DISTINCT rel.~A AS property" property-name) (relationshipid))))
    (query-statement statement)))

(def-neo4j-query-fun set-relationship-property () (relationshipid property value)
  ;; property name cannot be parametrized
  (let* ((property-name (string-sanitize property))
         (statement (make-statement (format nil "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} SET rel.`~A`={VALUE}" property-name) (relationshipid value))))
    (query-statement statement)))

(def-neo4j-query-fun del-relationship-property () (relationshipid property)
  ;; property name cannot be parametrized
  (let* ((property-name (string-sanitize property))
         (statement (make-statement (format nil "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} REMOVE rel.~A" property-name) (relationshipid))))
    (query-statement statement)))

(def-neo4j-query-fun get-relationship-properties (:extract-keys ("properties") :apply-fun 'caar) (relationshipid)
  (let* ((statement (make-statement "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} RETURN DISTINCT properties(rel) AS properties" (relationshipid))))
    (query-statement statement)))

(def-neo4j-query-fun set-relationship-properties () (relationshipid properties)
  (let* ((statement (make-statement "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} SET rel = {PROPERTIES}" (relationshipid properties))))
    (query-statement statement)))

(def-neo4j-query-fun del-relationship-properties () (relationshipid)
  (let* ((statement (make-statement "MATCH ()-[rel]-() WHERE id(rel)={RELATIONSHIPID} SET rel = {}" (relationshipid))))
    (query-statement statement)))

(def-neo4j-query-fun query-label () (labelname key value)
  (assert (or (and (null key) (null value))
              (and key value)))
  (let* ((label-name (string-sanitize labelname))
         (property-name (string-sanitize key))
         (where-clause (if (and property-name (string/= property-name ""))
                           (format nil "WHERE n.~A=~A" property-name value)
                           ""))
         (statement (make-statement (format nil "MATCH (n:~A) ~A RETURN id(n) AS id, labels(n) AS labels, properties(n) AS properties" label-name where-clause) ())))
    (query-statement statement)))

(def-neo4j-query-fun get-node-property () (nodeid property)
  ;; property name cannot be parametrized
  (let* ((property-name (string-sanitize property))
         (statement (make-statement (format nil "MATCH (n) WHERE id(n)={NODEID} RETURN DISTINCT n.~A AS property" property-name) (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun set-node-property () (nodeid property value)
  ;; property name cannot be parametrized
  (let* ((property-name (string-sanitize property))
         (statement (make-statement (format nil "MATCH (n) WHERE id(n)={NODEID} SET n.`~A`={VALUE}" property-name) (nodeid value))))
    (query-statement statement)))

(def-neo4j-query-fun del-node-property () (nodeid property)
  ;; property name cannot be parametrized
  (let* ((property-name (string-sanitize property))
         (statement (make-statement (format nil "MATCH (n) WHERE id(n)={NODEID} REMOVE n.~A" property-name) (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun get-node-properties (:extract-keys ("properties") :apply-fun 'caar) (nodeid)
  (let* ((statement (make-statement "MATCH (n) WHERE id(n)={NODEID} RETURN DISTINCT properties(n) AS properties" (nodeid))))
    (query-statement statement)))

(def-neo4j-query-fun set-node-properties () (nodeid properties)
  (let* ((statement (make-statement "MATCH (n) WHERE id(n)={NODEID} SET n = {PROPERTIES}" (nodeid properties))))
    (query-statement statement)))

(def-neo4j-query-fun del-node-properties () (nodeid)
  (let* ((statement (make-statement "MATCH (n) WHERE id(n)={NODEID} SET n = {}" (nodeid))))
    (query-statement statement)))
