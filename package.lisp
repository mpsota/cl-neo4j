(in-package #:cl-user)

(defpackage #:cl-neo4j
  (:nicknames #:neo)
  (:use #:cl)
  (:export #:init-connections
           #:with-transaction
           #:standard-node
           #:composite-node
           #:query))
  ;;(:shadow #:query) ;; Simpler query, query as a string. TODO rename?
  #+nil(:import-from #:cl-neo4j ;; some basic macros, should be also available (and exported) from the wrapper
                #:basic-handler
                #:with-request-handler
                #:set-request-handler
                #:neo4j-response-to-hashtable
                #:query
                #:query-statement
                #:with-transaction
                )
  #+nil(:export #:with-request-handler ;; cl-neo4j functions
           #:basic-handler
           #:set-request-handler

           #:query
           #:query-statement
           #:with-transaction

           ;; wrapper

           #:create
           #:node-get-by-id
           #:node-delete
           #:node-properties
           #:node-property
           #:node-relationships
           #:node-add-to-index
           #:node-remove-from-index
           #:node-query-index
           #:node-query-label
           #:node-traverse

           #:relationship-create
           #:relationship-get-by-id
           #:relationship-delete
           #:relationship-start
           #:relationship-end
           #:relationship-start-id
           #:relationship-end-id
           #:relationship-type
           #:relationship-properties
           #:relationship-property
           #:relationship-add-to-index
           #:relationship-remove-from-index
           #:relationship-query-index
           #:relationship-traverse

           #:standard-node
           #:standard-relationship

           #:node-id
           #:relationship-id

           #:node-labels
           ;; Vars
           #:*default-node-constructor*
           #:*default-relationship-constructor*
           )
