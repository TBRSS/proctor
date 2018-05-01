;;;; proctor.asd

(asdf:defsystem #:proctor
  :description "Tests as build targets."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:overlord
               #:alexandria
               #:serapeum)
  :components ((:file "package")
               (:file "api")
               (:file "results")
               (:file "proctor")
               (:file "assertions")
               (:file "printing")))
