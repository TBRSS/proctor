;;;; proctor.lisp

(in-package #:proctor)

(defun run (test)
  (~>> test
       get-test-results
       (print-test-results test)))

;; (defmacro define-suite (name &key in description)
;;   (let ((file (test-result-file name)))
;;     `(progn
;;        (save-suite ',name
;;                    :in ',in
;;                    :description ',description)
;;        (overlord:file-target ,name ,file ()
;;          (run-test-to-file (find-test ',name))))))

;; (defmacro in-suite (name)
;;   `(setf (current-suite)
;;          (find-suite ',name)))

(defmacro define-test (test-name &body body)
  (nest
   (destructuring-bind (test-name &key in)
       (ensure-list test-name))
   (let ((file (test-result-file test-name))))
   `(progn
      (save-test ',test-name
                 (lambda ()
                   ,@body)
                 :in (or ,in (current-suite)))
      (overlord:file-target ,test-name ,file ()
        (run-test-to-file (find-test ',test-name)
                          ,file))
      ',test-name)))

(defmacro is (test)
  `(is* (test-form ,test)))

(defmacro is-true (test)
  `(is (true ,test)))

(defmacro is-false (test)
  `(is (not ,test)))

(defmacro signals (condition-name &body body)
  `(signals*
    ',condition-name
    (test-form
     ,@body)))

(defmacro finishes (&body body)
  `(finishes*
    (test-form
      ,@body)))



(defgeneric run-test-to-string (test))
(defgeneric run-test (test))

(defmacro test-form (&body body)
  `(make-test-form :form ',(if (single body) (first body) body)
                   :function (lambda () ,@body)))

(defun get-test-results (test)
  (~> test
      build-result-file
      read-object-from-file))

(defun build-result-file (test)
  (let ((target (make-test-target test)))
    (overlord:build target))
  (test-result-file test))

(defun make-test-target (test)
  (test-result-file test))

(defvar *test-output*
  (make-synonym-stream '*standard-output*))
(declaim (type output-stream *test-output*))

(defun run-test-to-file (test file)
  (let* ((result (run-test-to-result test))
         (string
           (with-standard-io-syntax
             (prin1-to-string result))))
    (overlord:write-file-if-changed string file)))

(defunion test-result
  pass
  (failure
   (random-state random-state)
   (description string)))

(defun run-test-to-result (test)
  (let ((random-state (make-random-state nil)))
    (let ((string (run-test-to-string test)))
      (if (emptyp string) pass
          (failure random-state string)))))

(defclass abstract-test ()
  ((name
    :type symbol
    :initarg :name
    :reader test-name)
   (docstring
    :type string
    :initarg :documentation
    :initarg :description
    :reader test-documentation)
   (parent
    :initform nil
    :reader test-parent))
  (:default-initargs
   :name (required-argument :name)
   :documentation "NO DOCS"))

(defmethod print-object ((self abstract-test) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s" (test-name self))))

(defvar *tests* (make-hash-table))

(defun find-test (test-name)
  (gethash (assure symbol test-name)
           *tests*))

(defun (setf find-test) (test test-name)
  (setf (gethash test-name *tests*)
        (assure abstract-test test)))

;; (defclass suite (abstract-test)
;;   ((table
;;     :type hash-table
;;     :initform (make-hash-table)
;;     :reader suite-table))
;;   (:documentation "A test suite."))

;; (defvar *suites*
;;   (make-hash-table))

;; (defun find-suite (name)
;;   (or (gethash name *suites*)
;;       (error "No such suite as ~a." name)))

;; (defun (setf find-suite) (value name)
;;   (check-type value suite)
;;   (check-type name symbol)
;;   (setf (gethash name *suites*) value))

;; (defgeneric (setf test-parent) (parent suite)
;;   (:method ((parent symbol) suite)
;;     (setf (test-parent suite)
;;           (ensure-suite parent)))
;;   (:method (parent (suite symbol))
;;     (setf (test-parent (ensure-suite suite))
;;           parent))
;;   (:method ((parent suite) (suite suite))
;;     (with-slots (current-parent) suite
;;       (synchronized ()
;;         (cond ((no current-parent)
;;                (setf current-parent parent
;;                      (gethash (test-name suite)
;;                               (suite-table parent))
;;                      suite))
;;               ((eql current-parent parent)
;;                parent)
;;               (t
;;                (let ((name (test-name suite)))
;;                  (remhash name (suite-table current-parent))
;;                  (nix current-parent)
;;                  (setf (test-parent suite) parent))))))))

;; (defun ensure-suite (name &key in documentation)
;;   (check-type name symbol)
;;   (assure suite
;;     (lret ((suite
;;             (ensure2 (gethash name *suites*)
;;               (make 'suite
;;                     :name name
;;                     :documentation documentation))))
;;       (when in
;;         (setf (test-parent suite) in)))))

;; (defvar *default-suite*
;;   (ensure-suite nil))

(defvar *suite* nil)

(defplace current-suite ()
  *suite*)

(defclass test (abstract-test)
  ((function
    :type function
    :initarg :function
    :reader test-function)
   (package
    :type package
    :initarg :package
    :reader test-package)
   (suite
    :type symbol
    :initarg :suite
    :initarg :in
    :reader test-suite))
  (:default-initargs
   :package *package*
   :function (required-argument :function))
  (:documentation "A single test case."))

(defun make-test (name fn &key in)
  (make 'test :name name :function fn :in in))

(defun save-test (name thunk &key in)
  (setf (find-test name)
        (make-test name thunk :in in)))

(defmethod run-test-to-string ((test test))
  (with-slots (package name function) test
    (with-output-to-string (*test-output*)
      (run-test test))))

(defmethod run-test ((test test))
  (with-slots (package function) test
    (let ((*package* package))
      (funcall function))))



(defstruct-read-only test-form
  (function :type function)
  form)

(defmethod print-object ((self test-form) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~a" (test-form-form self))))

(defmethod run-test-form (test-form)
  (funcall (test-form-function test-form)))

(defun is* (form)
  (handler-case
      (unless (run-test-form form)
        (format *test-output*
                "~&Test ~a failed."
                form))
    (serious-condition (c)
      (format *test-output*
              "~&Test ~a failed because of error ~a."
              form c))))

(defun signals* (condition-type form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (format *test-output*
                             "~&Test ~a failed to complete because of error ~a"
                             form
                             c)
                     (return-from signals* nil)))
                 (t
                   (lambda (c)
                     (when (typep c condition-type)
                       (return-from signals* nil)))))
    (run-test-form form))

  (format *test-output*
          "~&Test ~a completed without signaling ~a"
          form
          condition-type))

(defun finishes* (form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (format *test-output*
                             "~&Test ~a did not finish:~%~a"
                             form
                             c))))
    (run-test-form form)))



(defun print-test-results (test result)
  (match-of test-result result
    (pass (format t "~&Test ~a: PASS" test))
    ((failure _ description)
     (format t "~&Test ~a: FAIL.~%~a"
             test
             description))))



(defconst result-ext "sexp")

(defgeneric test-result-file (test)
  (:method ((test abstract-test))
    (test-result-file (test-name test)))
  (:method ((test symbol))
    (let* ((package (symbol-package test))
           (package-name
             (package-name package))
           (symbol-name
             (symbol-name test)))
      (path-join
       (user-homedir-pathname)
       (make-pathname
        :directory `(:relative
                     "proctor"
                     ,(uiop:implementation-identifier)
                     ,package-name)
        :name symbol-name
        :type result-ext)))))

(defun read-object-from-file (file)
  (with-input-from-file (in file :element-type 'character)
    (with-standard-io-syntax
      (read in))))

(defun print-backtrace-to-string (error)
  (with-output-to-string (s)
    (uiop:print-backtrace
     :stream s
     :condition error)))
