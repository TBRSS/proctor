;;;; proctor.lisp

(in-package #:proctor)

;;; How tests results are stored.

(defunion test-result
  (pass
   (test-name symbol))
  (failure
   (test-name symbol)
   (plist list))
  (suite-result
   (test-name symbol)
   (test-results list)))

(defun passed? (test-result)
  (etypecase-of test-result test-result
    (pass t)
    (failure nil)
    (suite-result
     (every #'passed? (suite-result-test-results test-result)))))

(defun failed? (test-result)
  (not (passed? test-result)))



(def proctor-dir
  (path-join
   (user-homedir-pathname)
   (make-pathname
    :directory `(:relative
                 "proctor"
                 ,(uiop:implementation-identifier)))))

(defgeneric run-test-to-string (test)
  (:method ((test symbol))
    (run-test-to-string (find-test test))))

(defgeneric run-test (test)
  (:method ((test symbol))
    (run-test (find-test test))))

(defmacro test-form (&body body)
  `(make-test-form :form ',(if (single body) (first body) body)
                   :function (lambda () ,@body)))

(defun get-test-results (test)
  (~> test
      build-result-file
      read-test-result-from-file))

(defun read-test-result-from-file (file)
  (assure test-result
    (read-object-from-file file)))

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
             (write-to-string result :readably t))))
    ;; If the test failed, it should always be redone.
    (when (failed? result)
      (overlord:redo-always))
    (overlord:write-file-if-changed string file)))

(defun run-test-to-result (test)
  (let ((random-state (make-random-state nil)))
    (let ((string (run-test-to-string test)))
      (assure test-result
        (if (emptyp string) (pass test)
            (failure (test-name test)
                     (list :random-state random-state
                           :description string)))))))

(defgeneric test-name (test)
  (:method ((test symbol))
    test))

(defclass abstract-test ()
  ((name
    :type symbol
    :initarg :name
    :reader test-name)
   (docstring
    :type (or string null)
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

(defun find-suite (name)
  (assure suite
    (find-test name)))

(defvar *suite* nil)

(defplace current-suite ()
  *suite*)

(defclass suite (abstract-test)
  ()
  (:documentation "A test suite."))

(defvar *suite->tests* (make-hash-table))
(defvar *test->suite* (make-hash-table))

(defun suite-tests (suite)
  (values (gethash (test-name suite) *suite->tests*)))

(defun test-suite (test)
  (values (gethash (test-name test) *test->suite*)))

(defun add-test-to-suite (test suite)
  (let ((test (test-name test))
        (suite (test-name suite)))
    (synchronized ()
      ;; Remove any old mapping.
      (when-let (old (pophash test *test->suite*))
        (removef (gethash old *suite->tests*) test))
      ;; Add the new mapping.
      (setf (gethash test *test->suite*) suite)
      (pushnew test (gethash suite *suite->tests*))))
  (values))

(defun save-suite (name &key in description)
  (add-test-to-suite name in)
  (setf (find-test name)
        (make 'suite
              :name name
              :description description)))

(defgeneric run-suite-to-file (suite file)
  (:method ((suite symbol) file)
    (run-suite-to-file (find-suite suite) file))
  (:method ((suite suite) file)
    (maybe-update-suite-tests-file suite)
    (overlord:depends-on (suite-tests-file suite))
    (let* ((tests (suite-tests suite))
           (result-files (mapcar #'test-result-file tests)))
      (overlord:depends-on-all result-files)
      (maybe-save-suite-results suite file result-files))))

(defun maybe-update-suite-tests-file (suite)
  (let* ((tests (suite-tests suite))
         (tests (stable-sort-new tests #'string<))
         (string
           ;; NB This doesn't need to be readable, it just needs to be
           ;; consistent.
           (with-output-to-string (s)
             (with-standard-io-syntax
               (do-each (test tests)
                 (format s "~s~%" test)))))
         (file (suite-tests-file suite)))
    (overlord:write-file-if-changed string file)))

(defun maybe-save-suite-results (suite file result-files)
  (let* ((forms (mapcar #'read-test-result-from-file result-files))
         (results (suite-result (test-name suite) forms))
         (pass? (passed? results))
         (string
           (with-standard-io-syntax
             (write-to-string results :readably t))))
    (unless pass?
      (overlord:redo-always))
    (overlord:write-file-if-changed string file)))

(defclass test (abstract-test)
  ((function
    :type function
    :initarg :function
    :reader test-function)
   (package
    :type package
    :initarg :package
    :reader test-package))
  (:default-initargs
   :package *package*
   :function (required-argument :function))
  (:documentation "A single test case."))

(defun make-test (name fn)
  (make 'test :name name :function fn))

(defun save-test (name thunk &key in)
  (add-test-to-suite name in)
  (setf (find-test name)
        (make-test name thunk)))

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
                "~&Assertion ~a failed."
                form))
    (serious-condition (c)
      (format *test-output*
              "~&Assertion ~a exited abnormally:~%~a"
              form c))))

(defun signals* (condition-type form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (format *test-output*
                             "~&Form ~a failed to complete because of error ~a"
                             form
                             c)
                     (return-from signals* nil)))
                 (t
                   (lambda (c)
                     (when (typep c condition-type)
                       (return-from signals* nil)))))
    (run-test-form form))

  (format *test-output*
          "~&Form ~a completed without signaling ~a"
          form
          condition-type))

(defun finishes* (form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (format *test-output*
                             "~&Form ~a did not finish:~%~a"
                             form
                             c))))
    (run-test-form form)))



(defun print-test-result (result)
  (match-of test-result result
    ((pass name)
     (format t "~&Test ~a: PASS" name))
    ((failure test
              (trivia:lambda-list
               &key description
               &allow-other-keys))
     (format t "~&Test ~a: FAIL.~%~a"
             test
             description))
    ((suite-result test results)
     (format t "Suite ~a: ~a."
             test
             (eif (every #'passed? results) "PASS" "FAIL"))
     (do-each (result results)
       (print-test-result result)))))



(defconst .sexp "sexp")
(defconst .tests "tests")

(defun test-related-file (test ext)
  (let* ((test (test-name test))
         (package (symbol-package test))
         (package-name
           (package-name package))
         (symbol-name
           (symbol-name test)))
    (ensure-directories-exist
     (path-join
      proctor-dir
      (make-pathname
       :directory `(:relative
                    ,package-name)
       :name symbol-name
       :type ext)))))

(defun test-result-file (test)
  (test-related-file test .sexp))

(defun suite-tests-file (suite)
  (test-related-file suite .tests))

(defun read-object-from-file (file)
  (with-input-from-file (in file :element-type 'character)
    (with-standard-io-syntax
      (read in))))

(defun print-backtrace-to-string (error)
  (with-output-to-string (s)
    (uiop:print-backtrace
     :stream s
     :condition error)))
