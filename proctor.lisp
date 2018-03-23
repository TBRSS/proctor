;;;; proctor.lisp

(in-package #:proctor)

(defun run (test &optional (printer (default-printer)))
  (~>> test
       get-test-results
       (print-test-results printer)))

(defun get-test-results (test)
  (~> test
      build-result-file
      read-object-from-file))

(defun build-result-file (test)
  (let ((target (make-test-target test)))
    (overlord:build target))
  (test-result-file test))

(defclass abstract-test ()
  ((name
    :type symbol
    :initarg :name
    :reader test-name)
   (docstring
    :type string
    :initarg :documentation
    :reader test-documentation))
  (:default-initargs
   :name (required-argument :name)
   :docstring "NO DOCS"))

(defmethod print-object ((self abstract-test) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s" (test-name self))))

(defclass suite (abstract-test)
  ((table
    :type hash-table
    :initform (make-hash-table)
    :reader suite-table))
  (:documentation "A test suite."))

(defvar *suites*
  (make-hash-table))

(defun find-suite (name)
  (gethash name *suites*))

(defun (setf find-suite) (value name)
  (check-type value suite)
  (check-type name symbol)
  (setf (gethash name *suites*) value))

(defun ensure-suite (name &key in)
  (check-type name symbol)
  (assure suite
    (let ((suite
            (ensure2 (gethash name *suites*)
              (make 'suite :name name))))
      (if (null in) suite
          (let ((parent (ensure-suite in)))
            (setf (gethash name (suite-table parent))
                  suite))))))

(defvar *default-suite*
  (ensure-suite nil))

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

(defun make-test (name fn &key in)
  (let* ((test (make 'test :name name :function fn))
         (suite (find-suite in)))
    (setf (gethash name (suite-table suite))
          test)))

(defmethods test (self function package)
  (:method run (self)
    (let ((*package* package))
      (funcall function))))

(defclass abstract-result ()
  ((name
    :initarg :name
    :type symbol
    :reader test-name)))

(defmethod print-object ((self abstract-result) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (test-name self))))

(defclass suite-result (abstract-result)
  ((results
    :type list
    :initarg :results
    :reader suite-test-results)))

(defgeneric test-result-plist (result))

(defclass test-result (abstract-result)
  ())

(defmethod print-object ((self test-result) stream)
  (print-readable-object (self stream :eval t)
    `(make ',(class-name-safe self)
           ,@(test-result-plist self))))

(defmethod test-result-plist ((self abstract-result))
  nil)

(defclass pass (test-result)
  ())

(defmethod test-result-plist ((self pass))
  `(:name ,(test-name self)))

(defclass fail (test-result)
  ((random-state
    :initarg :random-state
    :type random-state)
   (failures
    :initarg :failures
    :type list)))

(defmethod test-result-plist ((self fail))
  (with-slots (random-state failures) self
    (list :random-state random-state
          :failures failures)))

(defclass fail/abnormal (fail)
  ((error
    :initarg :error
    :type error
    :reader test-result.error)
   (backtrace
    :initarg :backtrace
    :type string
    :reader test-result.backtrace)))

(defmethod test-result-plist ((self fail/abnormal))
  (with-slots (error backtrace) self
    (append
     (call-next-method)
     (list :error error
           :backtrace backtrace))))

(defclass test-result-printer ()
  ((stream
    :initarg :stream
    :type stream
    :reader printer-stream))
  (:default-initargs
   :stream (make-synonym-stream '*standard-output*)))

(defvar *default-printer*
  (make 'test-result-printer))

(defun default-printer ()
  *default-printer*)

(defgeneric print-test-result (printer result))

(defmethods test-result-printer (self stream)
  (:method print-test-result (self (result suite-result))
    (do-each (result (suite-test-results self))
      (print-test-result self result)))
  (:method print-test-result (self (result pass))
    (format stream "."))
  (:method print-test-result (self (result fail))
    (format stream "~&~a failed.~%"
            (test-name result)))
  (:method print-test-result (self (result fail/abnormal))
    (format stream "~%~a failed abnormally with an error of type ~a.~%"
            (test-name result)
            (class-name-of (test-result.error result)))))



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
       (overlord/cache:current-cache-dir)
       (make-pathname
        :directory `(:relative "proctor" ,package-name))
       (make-pathname
        :name symbol-name
        :type "sexp")))))

(defun read-object-from-file (file)
  (with-input-from-file (in file :element-type 'character)
    (with-standard-io-syntax
      (read in))))

(defun print-backtrace-to-string (error)
  (with-output-to-string (s)
    (uiop:print-backtrace
     :stream s
     :condition error)))
