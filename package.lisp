;;;; package.lisp

(defpackage #:proctor
  (:use #:cl #:alexandria #:serapeum)
  (:import-from #:uiop #:file-exists-p)
  (:export

   ;; Running tests.
   #:run
   #:runq
   #:debug-test

   ;; Defining tests and suites.
   #:test
   #:def-suite
   #:in-suite

   ;; Assertions.
   #:is
   #:is-true
   #:is-false
   #:signals
   #:finishes

   ;; Restarts.
   #:retry-test

   ;; Etc.
   #:failure-data))

(defpackage #:proctor-user
  (:use #:cl #:proctor))
