(in-package :cl-neo4j.tests)

;; (encode-neo4j-json-payload (list q q2) :statements)

;; "{\"statements\":[{\"statement\":\"CREATE INDEX ON :Foo(name)\"},{\"statement\":\"CREATE INDEX ON :Vulnerability(name)\",\"parameters\":{\"props\":{\"asdf\":\"asdf\"}}}]}"

;; (setf q2 (make-instance 'cypher-query :statement "CREATE INDEX ON :Foo(name)" :properties '(("asdf" . "asdf"))))
;; (setf q (make-instance 'cypher-query :statement "CREATE INDEX ON :Foo(name)"))
