;;;; proctor.lisp

(in-package #:proctor)

(defgeneric run (test))

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
   :name (required-argument :name)))

(defmethod print-object ((self abstract-test) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s" (test-name self))))

(defclass suite (abstract-test)
  ((table
    :type hash-table
    :initform (make-hash-table)
    :initarg :tests
    :reader suite-table))
  (:documentation "A test suite."))

(defmethods suite (self table)
  (:method run (self)
    (let ((tests (hash-table-values table)))
      (do-each (test (reshuffle tests))
        (run test)))))

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
   :package *package*)
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
  ((test-name
    :initarg :test-name
    :type symbol
    :reader result.test-name)))

(defclass suite-result (abstract-result)
  ((plan :type wholenum)))

(defclass test-result (abstract-result)
  ())

(defclass pass (test-result)
  ())

(defclass fail (test-result)
  ())
