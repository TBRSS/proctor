(in-package #:proctor)

(deftype format-control ()
  '(or string function))

(defstruct-read-only test-form
  (function :type function)
  form
  (reason-args :type list))

(defmacro test-form (form &rest reason-args)
  `(make-test-form
    :form ',form
    :function (lambda ()
                ,form)
    :reason-args ,(if (null reason-args) nil
                      (destructuring-bind (control . args) reason-args
                        `(list (formatter ,control) ,@args)))))

(defmethod print-object ((self test-form) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~a" (test-form-form self))))

(defun report (default-control form &rest default-args)
  (check-type form test-form)
  (check-type default-control format-control)
  (handler-bind ((serious-condition
                   (lambda (c)
                     (error "Error while reporting: ~a" c))))
    (restart-case
        (let ((reason-args (test-form-reason-args form)))
          (multiple-value-bind (control args)
              (if reason-args
                  (values (first reason-args)
                          (rest reason-args))
                  (values default-control
                          (cons form default-args)))
            (if *debug-test*
                (break "~?" control args)
                (format *test-output*
                        "~?"
                        control args))))
      (continue ()
        :test (lambda (c)
                (declare (ignore c))
                *debug-test*)
        :report "Record the error and continue."
        (let ((*debug-test* nil))
          (apply #'report default-control form default-args))))))

(define-compiler-macro report (&whole call control form &rest args)
  (if (stringp control)
      `(report (formatter ,control) ,form ,@args)
      call))

(defun call/retry (fn)
  (nlet lp ()
    (restart-case
        (funcall fn)
      (retry-test ()
        :test (lambda (c) (declare (ignore c))
                (boundp '*debug-test*))
        :report "Retry the assertion."
        (lp)))))

(defmacro with-retry ((&key) &body body)
  (with-thunk (body)
    `(call/retry ,body)))

(defmethod run-test-form (test-form)
  (funcall (test-form-function test-form)))

(defun is* (form)
  (with-retry ()
    (handler-bind ((serious-condition
                     (lambda (c)
                       (report "~&Assertion ~a exited abnormally:~%~a"
                               form c)
                       (return-from is* (values)))))
      (unless (run-test-form form)
        (report "~&Assertion ~a failed."
                form)))))

(defun signals* (condition-type form &rest args)
  (let ((control (first args))
        (args    (rest args)))
    (with-retry ()
      (handler-bind ((serious-condition
                       (lambda (c)
                         (unless (typep c condition-type)
                           (if control
                               (report "~?" control args)
                               (report "~
~&Form ~a failed to complete because of a condition of type ~a (expected ~a): ~a"
                                       form
                                       (type-of c)
                                       condition-type
                                       c)))
                         (return-from signals* nil))))
        (run-test-form form))

      (report "~&Form ~a completed without signaling ~a"
              form
              condition-type))))

(defun finishes* (form)
  (with-retry ()
    (handler-bind ((serious-condition
                     (lambda (c)
                       (report "~&Form ~a did not finish:~%~a"
                               form
                               c))))
      (run-test-form form))))
