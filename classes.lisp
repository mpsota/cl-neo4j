(in-package #:cl-neo4j)

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

(defclass composite-node (standard-node)
  ()
  (:documentation "Similar to standard-node, but could be build from few different nodes/ properties. Node of this class doesn't have node-id nor node-labels, as it is not created just from 1 node (or might not be)."))

(defmethod node-id ((node composite-node))
  (error "No node-id for composite-node - node is the composition of different nodes, with different node ids"))

(defmethod node-labels ((node composite-node))
  (error "No node-labels for composite-node - node is the composition of different nodes, with different node labels"))

(defmethod print-object ((object composite-node) stream)
  (print-unreadable-object (object stream :type t :identity '())
    (format stream "<NO NODE-ID>")))
