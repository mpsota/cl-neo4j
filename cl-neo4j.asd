;; ASDF package description for cl-neo4j              -*- Lisp -*-

(defpackage :cl-neo4j-system (:use :cl :asdf))
(in-package :cl-neo4j-system)

(defsystem cl-neo4j
  :name "neo4j RESTful Client Interface ()"
  :maintainer "Mikhail Novikov <freiksenet@gmail.com>, Equill <https://github.com/equill>, Michał Psota <michal@lisp.pl>"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.4"
  :description "neo4j RESTful Client Interface"
  :long-description "neo4j RESTful Client Interface and higher order extensible CL wrapper for it."
  :depends-on (:alexandria
               :anaphora
               :split-sequence
               :drakma
               :babel
               :cl-json
               :cl-ppcre)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "globals")
             (:file "query")
             (:file "utilities" :depends-on ("query"))
             (:file "conditions" :depends-on ("utilities"))
             (:file "requests" :depends-on ("conditions" "query" "globals"))
             (:file "restapi" :depends-on ("requests"))
             (:file "wrapper-transaction" :depends-on ("utilities" "restapi"))
             (:file "query-restapi" :depends-on ("utilities" "wrapper-transaction"))
             (:file "wrapper" :depends-on ("wrapper-transaction" "query-restapi"))
             ))))

(defsystem cl-neo4j.tests
  :depends-on  (:cl-neo4j
                :fiveam)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "package")
             (:file "util")
             (:file "main")
             (:file "restapi")
             (:file "wrapper")))))
