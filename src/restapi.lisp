;;; Low-level REST api for the database. Returns raw unserialized responses.

(in-package #:cl-neo4j)

(def-neo4j-fun transaction-begin ()
  :post
  (:uri-spec "transaction")
  (:encode "{ \"statements\": [] }"
           :raw-string)
  (:status-handlers
   (201 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))))

(def-neo4j-fun transaction-keep-alive (transaction)
  :post
  (:uri-spec (format nil "transaction/~D" transaction))
  (:encode "{ \"statements\": [] }"
           :raw-string)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))
   (404 (error 'no-such-transaction :transaction transaction))
   ))

(def-neo4j-fun transaction-commit (transaction)
  :post
  (:uri-spec (format nil "transaction/~D/commit" transaction))
  (:encode "{ \"statements\": [] }"
           :raw-string)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))
   (404 (error 'no-such-transaction :transaction transaction))
   ))

(def-neo4j-fun transaction-rollback (transaction)
  :delete
  (:uri-spec (format nil "transaction/~D" transaction))
  (:encode "{ \"statements\": [] }"
           :raw-string)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))
   (404 (error 'no-such-transaction :transaction transaction))
   ))

(def-neo4j-fun transaction-with-commit (statements)
  :post
  (:uri-spec "transaction/commit")
  (:encode statements :alist)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))))

(def-neo4j-fun cypher-query (statements)
  :post
  (:uri-spec "transaction/commit")
  (:encode statements :statements)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))
   (404 (error 'node-not-found-error :uri uri))))

(def-neo4j-fun cypher-query-in-transaction (statements transaction)
  :post
  (:uri-spec (format nil "transaction/~D" transaction))
  (:encode statements :statements)
  (:status-handlers
   (200 (decode-neo4j-json-output body))
   (401 (error 'unauthorised-error))
   (404 (error 'node-not-found-error :uri uri))))
