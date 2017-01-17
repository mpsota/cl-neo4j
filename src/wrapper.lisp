;; Higher lever wrapper for neo4j REST. Users need to define at least node-id and relationship-id accessors for portability + constructors.

(in-package #:cl-neo4j-wrapper)

;;; Protocol

;; Those variables define default return type for factory methods

(defvar *default-node-constructor*
  'make-standard-node)

(defvar *default-relationship-constructor*
  'make-standard-relationship)

;; Nodes

(defun create (labels properties &key initial-connections initial-indexes (constructor *default-node-constructor*))
  "Creates a new node in the graph. Returns created node.

   constructor is a constructor for nodes. By default it uses *default-node-constructor*.
   properties are plist of node properties.
   initial-connections are list of lists of node (or ids), type and direction that node should be connected to.
   initial-indexes are list of lists of index name, key and value of indexes that node should be added to.
  label"
  (setf labels (ensure-list labels))
  (let* ((node (funcall constructor (create-node :properties (plist-alist properties)))))
    (mapc (lambda (label)
            (set-node-label :nodeid (node-id node) :label label)) labels)
    (mapc (curry #'apply
                   (lambda (target type direction)
                     (relationship-create node target type :direction direction)))
          initial-connections)
    (mapc (curry #'apply
                 (lambda (index key value)
                   (node-add-to-index node index key value)))
          initial-indexes)
    ;; ask again for fresh data
    (funcall constructor (get-node :nodeid (node-id node)))))

(defun node-get-by-id (id &key (constructor *default-node-constructor*))
  "Returns a node with given id or nil otherwise. This is a factory method, it accepts keyword argument constructor whcih is defaulted to *default-node-constructor*"
  (funcall constructor (get-node :nodeid id)))

(defgeneric node-delete (node &key)
  (:documentation "Deletes a node. Cascade deletes the node even if node had relationships (deleting them too).")
  (:method ((node integer) &key cascade)
    (when cascade
      (mapc #'relationship-delete (node-relationships node)))
    (delete-node :node-id node)))

(defgeneric node-properties (node)
  (:documentation "Returns plist of properties of the node.")
  (:method ((node integer))
    (get-node-properties :node-id node)))

(defgeneric node-property (node property)
  (:documentation "Returns value of the property of the node. Returns nil if property is undefined.")
  (:method ((node integer) property)
    (get-node-property :node-id node :property property)))

(defgeneric (setf node-property) (value node property)
  (:documentation "Sets a value of the property of the node to value. Value of nil deletes property.")
  (:method (value (node integer) property)
    (if value
        (set-node-property :node-id node :property property :value value)
        (del-node-property :node-id node :property property))))

(defgeneric node-relationships (node &key types direction)
  (:documentation "Returns list of node relations, optionally filtered by list of possible types and direction. This is a factory method, accepts *default-node-constructor*")
  (:method ((node integer) &key (constructor *default-relationship-constructor*) types direction)
    (mapcar constructor
            (get-node-relationships :nodeid node :types types :direction direction))))

(defgeneric node-add-to-index (node index key value)
  (:documentation "Adds node to index with key and value.")
  (:method ((node integer) index key value)
    (add-to-index :type :node
                  :name index
                  :key key
                  :value value
                  :objectid node)))

(defgeneric node-remove-from-index (node index &optional key value)
  (:documentation "Removes node from index. Optionally removes only from index with key and value.")
  (:method ((node integer) index &optional key value)
    (remove-from-index :type :node
                       :name index
                       :key key
                       :value value
                       :object-id node)))

(defun node-query-index (index key value &key (constructor *default-node-constructor*))
  "Returns list of nodes in the index with key and value.

  This is a factory method, it accepts keyword argument constructor which is defaulted to *default-node-constructor*"
  (handler-case
      (mapcar constructor (lookup-index :type :node
                                        :name index
                                        :key key
                                        :value value))
    (index-entry-not-found-error () nil)))

(defun node-query-label (label key value &key (constructor *default-node-constructor*))
  "Returns list of nodes in the index with key and value.

  This is a factory method, it accepts keyword argument constructor which is defaulted to *default-node-constructor*"
  (declare (type string key))
  (handler-case
      (mapcar constructor (query-label
                           :labelname label
                           :key key
                           :value (format nil "~s" value)))
    (index-entry-not-found-error () nil)))

(defgeneric node-traverse (node &key order uniqueness relationships prune-evaluator return-filter max-depth)
  (:documentation "")
  (:method ((node integer) &rest keys &key (constructor *default-node-constructor*) &allow-other-keys)
    (mapcar constructor (apply #'traverse :node-id node :return-type :node keys))))

;; Relationships

(defgeneric relationship-create (node1 node2 type &key properties direction initial-indexes)
  (:documentation
   "Creates new relationship in a graph. Returns created relationship. If any step fails it tries to cleanup after itself.

    constructor is a constructor for relationships. By default it uses *default-relationship-constructor*.
    properties are plist of relationship properties.
    direction indicates a direction of relation from the point of view of node1.
    initial-indexes are list of lists of index name, key and value of indexes that node should be added to")
  (:method ((node1 integer) (node2 integer) type &key (constructor *default-relationship-constructor*) properties (direction :from) initial-indexes)
    (if (eq direction :all)
        (progn
          (relationship-create node2 node1 type :properties properties :direction :from :initial-indexes initial-indexes)
          (relationship-create node1 node2 type :properties properties :direction :from :initial-indexes initial-indexes))
        (destructuring-bind (start end)
            (case direction
              (:from (list node1 node2))
              (:to (list node2 node1)))
          (let ((relationship (funcall constructor
                                       (create-relationship :nodeid1 start
                                                            :nodeid2 end
                                                            :type type
                                                            :properties (plist-alist properties)))))
            (mapc (curry #'apply
                         (lambda (index key value)
                           (relationship-add-to-index relationship index key value)))
                  initial-indexes)
            relationship)))))

(defun relationship-get-by-id (id &key (constructor *default-node-constructor*))
  "Returns relationship with given id or nil otherwise."
  (funcall constructor (get-relationship :relationship-id id)))

(defgeneric relationship-delete (relationship)
  (:documentation "Deletes a relationship.")
  (:method ((relationship integer))
    (delete-relationship :relationshipid relationship)))

(defgeneric relationship-start (relationship)
  (:documentation "Returns start node of relationship.")
  (:method ((relationship integer))
    (relationship-start-id (make-standard-relationship (get-relationship :relationshipid relationship)))))

(defgeneric relationship-end (relationship)
  (:documentation "Returns end node of relationship.")
  (:method ((relationship integer))
    (relationship-end-id (make-standard-relationship (get-relationship :relationshipid relationship)))))

(defgeneric relationship-type (relationship)
  (:documentation "Returns type of the relationship.")
  (:method ((relationship integer))
    (relationship-type (make-standard-relationship (get-relationship :relationshipid relationship)))))

(defgeneric relationship-properties (relationship)
  (:documentation "Returns plist of relationship properties.")
  (:method ((relationship integer))
    (relationship-properties (make-standard-relationship (get-relationship :relationshipid relationship)))))

(defgeneric relationship-property (relationship property)
  (:documentation "Returns value of the property of the relationship. Returns nil if property is undefined.")
  (:method ((relationship integer) property)
    (gethash "property"
             (get-relationship-property :relationshipid relationship
                                        :property property))))

(defgeneric (setf relationship-property) (value relationship property)
  (:documentation "Sets a value of the property of the relationship to value. Value of nil deletes property.")
  (:method (value (relationship integer) property)
    (if value
        (set-relationship-property :relationshipid relationship :property property :value value)
        (del-relationship-property :relationshipid relationship :property property))))

(defgeneric relationship-add-to-index (relationship index key value)
  (:documentation "Adds Relationship to index with key and value.")
  (:method ((relationship integer) index key value)
    (add-to-index :type :relationship
                  :name index
                  :key key
                  :value value
                  :object-id relationship)))

(defgeneric relationship-remove-from-index (relationship index &optional key value)
  (:documentation "Removes relationship from index. Optionally removes only from index with key and value.")
  (:method ((relationship integer) index &optional key value)
    (remove-from-index :type :relationship
                       :name index
                       :key key
                       :value value
                       :object-id relationship)))

(defun relationship-query-index (index key value &key (constructor *default-relationship-constructor*))
  "Returns list of nodes in the index with key and value."
  (handler-case
      (mapcar constructor (lookup-index :type :relationship
                                        :name index
                                        :key key
                                        :value value))
    (index-entry-not-found-error () nil)))

(defgeneric relationship-traverse (node &key order uniqueness relationships prune-evaluator return-filter max-depth)
  (:documentation "")
  (:method ((node integer) &rest keys &key (constructor *default-relationship-constructor*) &allow-other-keys)
    (mapcar constructor (apply #'traverse :node-id node :return-type :relationship keys))))


;;; Implementation

(defun extract-id-from-link (link)
  (parse-integer (car (split-sequence #\/ link :from-end t :count 1))))

(defun normalize-alist (alist)
  (mapcar (lambda (el)
            (cons (json:lisp-to-camel-case (symbol-name (car el)))
                  (cdr el)))
          alist))

(defclass standard-node ()
  ((id
    :initarg :id
    :accessor node-id)
   (properties
    :initarg :properties
    :accessor node-properties)
   (labels
    :initarg :labels
    :accessor node-labels)))

(defmethod print-object ((object standard-node) stream)
  (print-unreadable-object (object stream :type t :identity '())
    (princ (node-id object) stream)))

(defun make-standard-node (ht)
  (make-instance 'standard-node
                 :id (gethash "id" ht)
                 :properties (normalize-alist (gethash "properties" ht))
                 :labels (gethash "labels" ht)))

(defun make-standard-node2 (data)
  ;; handle both already filtered data and raw
  (let ((data (if (cdr (assoc :results data))
                  (cadr (assoc :data (cadr (assoc :results data))))
                  data)))
    (when data
      (let* ((meta (cadr (assoc :meta data)))
             (row (cadr (assoc :row data)))
             (type (cdr (assoc :type meta))))
        (when row
          (cond
            ((string= type "node")
             (let ((id (cdr (assoc :id meta))))
               (neo:node-get-by-id id)))
            (t (error (format nil "Unknown item type returned from neo4j: `~A'" type)))))))))

(defmethod node-delete ((node standard-node) &key cascade)
  (node-delete (node-id node) :cascade cascade))

(defmethod node-property ((node standard-node) property)
  (cdr (assoc property (node-properties node) :test #'equal)))

(defmethod (setf node-property) (value (node standard-node) property)
  (setf (node-property (node-id node) property) value)
  (setf (node-properties node) (normalize-alist (get-node-properties :node-id (node-id node)))))

(defmethod node-relationships ((node standard-node) &key types direction)
  (node-relationships (node-id node) :types types :direction direction))

(defmethod node-add-to-index ((node standard-node) index key value)
  (node-add-to-index (node-id node) index key value))

(defmethod node-remove-from-index ((node standard-node) index &optional key value)
  (node-remove-from-index (node-id node) index key value))

(defmethod node-traverse ((node standard-node) &rest keys)
  (apply #'node-traverse (node-id node) keys))

(defclass standard-relationship ()
  ((id
    :initarg :id
    :accessor relationship-id)
   (properties
    :initarg :properties
    :accessor relationship-properties)
   (type
    :initarg :type
    :accessor relationship-type)
   (start-id
    :initarg :start
    :accessor %relationship-start-id)
   (end-id
    :initarg :end
    :accessor %relationship-end-id)))

(defmethod print-object ((object standard-relationship) stream)
  (print-unreadable-object (object stream :type t :identity '())
    (format stream "~A [~A--~A->~A]"
            (relationship-id object)
            (%relationship-start-id object)
            (relationship-type object)
            (%relationship-end-id object))))

(defun make-standard-relationship (ht)
  (make-instance 'standard-relationship
                 :id (gethash "id" ht)
                 :properties (normalize-alist (gethash "properties" ht))
                 :type (gethash "type" ht)
                 :start (gethash "startnodeid" ht)
                 :end (gethash "endnodeid" ht)))

(defmethod relationship-create ((node1 standard-node) (node2 standard-node) type &rest keys)
  (apply #'relationship-create (node-id node1) (node-id node2) type keys))

(defmethod relationship-delete ((relationship standard-relationship))
  (relationship-delete (relationship-id relationship)))

(defmethod relationship-start ((relationship standard-relationship))
  (node-get-by-id (%relationship-start-id relationship)))

(defmethod relationship-end ((relationship standard-relationship))
  (node-get-by-id (%relationship-end-id relationship)))

(defmethod relationship-start-id ((relationship standard-relationship))
  (%relationship-start-id relationship))

(defmethod relationship-end-id ((relationship standard-relationship))
  (%relationship-end-id relationship))

(defmethod relationship-property ((relationship standard-relationship) property)
  (cdr (assoc property (relationship-properties relationship) :test #'equal)))

(defmethod (setf relationship-property) (value (relationship standard-relationship) property)
  (setf (relationship-property (relationship-id relationship) property) value)
  (setf (relationship-properties relationship)
        (normalize-alist (get-relationship-properties :relationship-id (relationship-id relationship)))))

(defmethod relationship-add-to-index ((relationship standard-relationship) index key value)
  (relationship-add-to-index (relationship-id relationship) index key value))

(defmethod relationship-remove-from-index ((relationship standard-relationship) index &optional key value)
  (relationship-remove-from-index (relationship-id relationship) index key value))

(defmethod relationship-traverse ((node standard-node) &rest keys)
  (apply #'relationship-traverse (node-id node) keys))
