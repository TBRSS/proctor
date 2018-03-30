(in-package #:proctor)

(defun run (test)
  (~>> test
       get-test-result
       print-test-result))

(defmacro runq (test)
  `(run ',test))

(defmacro define-test (test-name &body body)
  (nest
   (destructuring-bind (test-name &key in)
       (ensure-list test-name))
   (let ((file (test-result-file test-name))))
   `(progn
      (save-test ',test-name
                 (lambda ()
                   (overlord:with-script ()
                     ,@body))
                 :in (or ,in (current-suite)))
      (overlord:file-target ,test-name ,file ()
        (run-test-to-file ',test-name ,file))
      ',test-name)))

(defmacro define-suite (name &body body)
  (nest
   (multiple-value-bind (opts dependencies)
       (parse-leading-keywords body))
   (destructuring-bind (&key in description) opts)
   (let ((file (test-result-file name)))
     `(progn
        (save-suite ',name
                    :in ',in
                    :description ',description)
        (overlord:file-target ,name ,file ()
          ,@dependencies
          (run-suite-to-file ',name ,file))
        ',name))))

(defmacro in-suite (name)
  `(setf (current-suite)
         (find-test ',name)))

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
