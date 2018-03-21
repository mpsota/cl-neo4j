;;; Conditions that low-level REST api throw in default unexpected situations.

(in-package #:cl-neo4j)

(define-condition unauthorised-error (error)
  ((status :accessor status :initarg :status))
  (:report (lambda (condition stream)
             (format stream "Access unauthorised: ~A" (status condition)))))

(define-condition unknown-return-type-error (error)
  ((uri :accessor uri :initarg :uri)
   (property :accessor property :initarg :property)
   (status :accessor status :initarg :status))
  (:report (lambda (condition stream)
             (format stream "Unknown status ~A returned for ~A"
                     (status condition) (uri condition)))))

(define-condition invalid-data-sent-error (error)
  ((json :accessor json :initarg :json)
   (uri :accessor uri :initarg :uri))
  (:report (lambda (condition stream)
             (format stream "Invalid data sent to ~A: ~A" (uri condition) (json condition)))))

(define-condition node-not-found-error (error)
  ((uri :accessor uri :initarg :uri)
   (property :accessor property :initarg :property))
  (:report (lambda (condition stream)
             (if (slot-boundp condition 'property)
                 (format stream "Property ~A not found for node ~A"
                         (property condition) (uri condition))
                 (format stream "Node not found ~A" (uri condition))))))

(define-condition unable-to-delete-node-error (error)
  ((uri :accessor uri :initarg :uri))
  (:report (lambda (condition stream)
             (format stream "Unable to delete node ~A. Still has relationships?" (uri condition)))))

(define-condition relationship-not-found-error (error)
  ((uri :accessor uri :initarg :uri)
   (property :accessor property :initarg :property))
  (:report (lambda (condition stream)
             (if (slot-boundp condition 'property)
                 (format stream "Property ~A not found for relationship ~A"
                         (property condition) (uri condition))
                 (format stream "Relationship not found ~A" (uri condition))))))

(define-condition property-not-found-error (error)
  ((uri :accessor uri :initarg :uri))
  (:report (lambda (condition stream)
             (format stream "Property not found at ~A"
                     (uri condition)))))

(define-condition index-not-found-error (error)
  ((uri :accessor uri :initarg :uri))
  (:report (lambda (condition stream)
             (format stream "Index not found ~A" (uri condition)))))

(define-condition index-entry-not-found-error (error)
  ((uri :accessor uri :initarg :uri))
  (:report (lambda (condition stream)
             (format stream "Index entry not found ~A" (uri condition)))))

(define-condition path-not-found-error (error)
  ((uri :accessor uri :initarg :uri))
  (:report (lambda (condition stream)
             (format stream "No path found with current algorithm at ~A" (uri condition)))))

(define-condition no-such-transaction (error)
  ((transaction :accessor transaction :initarg :transaction))
  (:report (lambda (condition stream)
             (format stream "No such transaction: ~D" (transaction condition)))))

;; BOLT conditions

;; FIXME only one condition now...

(define-condition bolt-error (error)
  ((code :accessor code :initarg :code)
   (message :accessor condition-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Error ~A: ~A" (code condition) (condition-message condition)))))
