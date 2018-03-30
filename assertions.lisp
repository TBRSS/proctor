(in-package #:proctor)

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
