;;;; package.lisp

(defpackage #:proctor
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:uiop #:file-exists-p)
  (:export #:run
           #:failure-data
           #:runq
           #:define-test
           #:define-suite
           #:in-suite

           #:is
           #:is-true
           #:is-false
           #:signals
           #:finishes))
