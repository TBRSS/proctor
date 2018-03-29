;;;; proctor.asd

(asdf:defsystem #:proctor
  :description "Conditional testing with Overlord"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:overlord
               #:alexandria
               #:serapeum)
  :components ((:file "package")
               (:file "api")
               (:file "proctor")))
