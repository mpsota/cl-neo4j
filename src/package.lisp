(in-package #:cl-user)

(defpackage #:cl-neo4j
  ;; (:nicknames #:neo4j)
  (:use #:cl
        #:alexandria
        #:anaphora
	#:json
	#:json-rpc
	#:drakma)
  (:export #:basic-handler
           #:transaction-begin
           #:transaction-keep-alive
           #:transaction-commit
           #:transaction-rollback
           #:transaction-with-commit
           #:get-node
           #:create-node
           #:delete-node
           #:set-node-label
           #:unauthorised-error
           #:set-node-properties
           #:get-node-properties
           #:del-node-properties
           #:set-node-property
           #:get-node-property
           #:del-node-property
           #:get-relationship
           #:create-relationship
           #:set-relationship-properties
           #:get-relationship-properties
           #:del-relationship-properties
           #:set-relationship-property
           #:get-relationship-property
           #:del-relationship-property
           #:delete-relationship
           #:get-node-relationships
           #:get-relationships-types
           #:create-index
           #:delete-index
           #:add-to-index
           #:remove-from-index
           #:lookup-index
           #:query
           #:query-index
           #:query-label
           #:cypher-query
           #:cypher-query-in-transaction
           #:traverse
           #:get-path
           #:get-paths
           ;; Conditions
           #:unknown-return-type-error
           #:invalid-data-sent-error
           #:node-not-found-error
           #:property-not-found-error
           #:unable-to-delete-node-error
           #:relationship-not-found-error
           #:index-not-found-error
           #:index-entry-not-found-error
           #:path-not-found-error
           #:*neo4j-host*
           ;; Vars
           #:*neo4j-port*
           #:*neo4j-user*
           #:*neo4j-pass*
           ;;
           #:with-request-handler
           #:set-request-handler))

(defpackage #:cl-neo4j-wrapper
  (:nicknames #:neo)
  (:use #:cl
        #:alexandria
        #:anaphora
        #:split-sequence
        #:cl-neo4j)
  (:shadow #:query) ;; Simpler query, query as a string. TODO rename?
  (:import-from #:cl-neo4j ;; some basic macros, should be also available (and exported) from the wrapper
                #:basic-handler
                #:with-request-handler
                #:set-request-handler
                )
  (:export #:with-request-handler ;; cl-neo4j functions
           #:basic-handler
           #:set-request-handler
           ;;
           #:query
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
                                        ;: Vars
           #:*default-node-constructor*
           #:*default-relationship-constructor*))
