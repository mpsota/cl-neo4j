;;; Neo4j requests and request handlers for them. Also database stuff is handled here.

(in-package :cl-neo4j)

(defvar *default-request-handler*)

(defmacro set-request-handler (handler)
  "globally binds `*default-request-handler*'"
  `(setf *default-request-handler* ,handler))

(defmacro with-request-handler (handler &body body)
  "evaluates body with `*default-request-handler*' set to handler"
  `(let ((*default-request-handler* ,handler))
     (unwind-protect (progn
                       ,@body)
       (close-handler *default-request-handler*))))

;; Requests and handlers

(defmacro def-neo4j-fun (name lambda-list method &rest args)
  ;; TODO use keywords instead of &rest args.
  `(defun ,name (&key (request-handler *default-request-handler*) ,@lambda-list)
     (let ((uri ,(cadr (assoc :uri-spec args)))
           (json ,(aif (assoc :encode args)
                        `(encode-neo4j-json-payload ,@(cdr it))
                        nil))) ;; (list '() :string) -> "null"
       (make-neo4j-request ,method uri json
                           (list ,@(mapcar (lambda (handler)
                                             `(list ,(car handler) (lambda (body uri json)
                                                                     (declare (ignorable body uri json))
                                                                     (progn
                                                                       ,@(cdr handler)))))
                                           (cdr (assoc :status-handlers args))))
                           :request-handler request-handler))))

(defstruct (neo4j-request (:constructor %make-neo4j-request)
                          (:conc-name request-))
  method
  uri
  payload)

(defun make-neo4j-request (method uri payload error-handlers &key (request-handler *default-request-handler*))
  (handle-request request-handler (%make-neo4j-request :method method
                                                       :uri uri
                                                       :payload payload)
                  error-handlers))

(defgeneric send-request (handler request)
  (:documentation "Governs how handler sends the request."))

(defgeneric handle-request (handler request error-handlers)
  (:documentation "Main interface for the handlers, make-neo4j-request uses it.")
  (:method ((handler symbol) request error-handlers)
    (handle-request (funcall handler) request error-handlers)))

(defgeneric close-handler (handler)
  (:documentation "Closes the handler. Handler should do finalization operarions - batch handler sends the request at this point."))

(defclass basic-handler ()
  ((host :initarg :host :accessor handler-host :initform *neo4j-host*)
   (port :initarg :port :accessor handler-port :initform *neo4j-port*)
   (user :initarg :user :accessor handler-user :initform *neo4j-user*)
   (pass :initarg :pass :accessor handler-pass :initform *neo4j-pass*))
  (:documentation "Basic handler that just sends request to the database."))

(defun basic-handler (&key (host *neo4j-host*) (port *neo4j-port*)
                        (user *neo4j-user*) (pass *neo4j-pass*))
  (make-instance 'basic-handler
                 :host host
                 :port port
                 :user user
                 :pass pass))

#+deprecated
(defmethod send-request* ((handler basic-handler) request)
  (with-accessors ((method request-method) (uri request-uri) (payload request-payload))
      request
    (multiple-value-bind (body status)
        (http-request (format-neo4j-query (handler-host handler)
                                          (handler-port handler)
                                          uri)
                      :method method
                      :protocol "http/1.1"
                      :basic-authorization (list (handler-user handler)
                                                 (handler-pass handler))
                      :content payload
                      :content-type "application/json"
                      :additional-headers '(("X-Stream" . "true"))
                      :accept "application/json; charset=UTF-8")
      (values status body))))

#+deprecated
(defmethod send-request** ((handler basic-handler) request)
  (with-accessors ((method request-method) (uri request-uri) (payload request-payload) (parameters request-parameters))
      request
    (multiple-value-bind (body status)
        (curl:)
        (curl:with-connection-returning-string (:cookies nil)
          (curl:set-option :url (format-neo4j-query (handler-host handler)
                                                    (handler-port handler)
                                                    uri))
          (curl:set-option :username (handler-user handler))
          (curl:set-option :password (handler-pass handler))
          (curl:set-header "Content-Type: application/json")
          (curl:set-header "X-Stream: true")
          (curl:set-option :postfields payload)
          (curl:perform))
      (values status body))))

(defmethod send-request ((handler basic-handler) request)
  (with-accessors ((method request-method) (uri request-uri) (payload request-payload))
      request
    (multiple-value-bind (body status)
        (curl:http-request (format-neo4j-query (handler-host handler)
                                               (handler-port handler)
                                               uri)
                           :method method
                           :content payload
                           :content-type "application/json"
                           :basic-authorization (list (handler-user handler)
                                                      (handler-pass handler))
                           :additional-headers '("X-Stream: true"))
      (values status body))))

(defmethod handle-request ((handler basic-handler) request error-handlers)
  (multiple-value-bind (status body) (send-request handler request)
    (aif (assoc status error-handlers)
         (funcall (second it)
                  body
                  (request-uri request)
                  (request-payload request))
         (error 'unknown-return-type-error
                :uri (request-uri request)
                :status status))))

(defmethod close-handler ((handler basic-handler))
  (declare (ignore handler))
  t)
