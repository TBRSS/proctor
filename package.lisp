;;;; package.lisp

(defpackage #:proctor
  (:use #:cl #:alexandria #:serapeum)
  (:export #:run
           #:runq
           #:define-test
           #:define-suite
           #:in-suite

           #:is
           #:is-true
           #:is-false
           #:signals
           #:finishes))
