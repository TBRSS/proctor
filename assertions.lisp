(in-package #:proctor)

(defun report (control &rest args)
  (if *debug-test*
      (break "~?" control args)
      (format *test-output*
              "~?"
              control args)))

(define-compiler-macro report (&whole call control &rest args)
  (if (stringp control)
      `(report (formatter ,control) ,@args)
      call))

(defstruct-read-only test-form
  (function :type function)
  form)

(defmacro test-form (&body body)
  `(make-test-form :form ',(if (single body) (first body) body)
                   :function (lambda () ,@body)))

(defmethod print-object ((self test-form) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~a" (test-form-form self))))

(defmethod run-test-form (test-form)
  (funcall (test-form-function test-form)))

(defun is* (form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (report "~&Assertion ~a exited abnormally:~%~a"
                             form c)
                     (return-from is* (values)))))
    (unless (run-test-form form)
      (report "~&Assertion ~a failed."
              form))))

(defun signals* (condition-type form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (unless (typep c condition-type)
                       (report "~
~&Form ~a failed to complete because of a condition of type ~a (expected ~a): ~a"
                               form
                               (type-of c)
                               condition-type
                               c))
                     (return-from signals* nil))))
    (run-test-form form))

  (report "~&Form ~a completed without signaling ~a"
          form
          condition-type))

(defun finishes* (form)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (report "~&Form ~a did not finish:~%~a"
                             form
                             c))))
    (run-test-form form)))
