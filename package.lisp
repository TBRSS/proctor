;;;; package.lisp

(defpackage #:proctor
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:uiop #:file-exists-p)
  (:export #:run
           #:failure-data
           #:runq
           #:test
           #:def-suite
           #:in-suite

           #:is
           #:is-true
           #:is-false
           #:signals
           #:finishes))

(defpackage #:proctor-user
  (:use #:cl #:proctor))
