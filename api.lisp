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
   (let ((file (test-result-file test-name))
         (suite (or in (test-name (current-suite))))))
   `(progn
      (save-test ',test-name
                 (lambda ()
                   (overlord:with-script ()
                     ,@body))
                 :in ',suite)
      (overlord:file-target ,test-name ,file ()
        (depend-on-suite-parents ',suite)
        (depend-on-suite-deps ',suite)
        (run-test-to-file ',test-name ,file))
      ',test-name)))

(defmacro define-suite (name &body body)
  (nest
   (multiple-value-bind (opts dependencies)
       (parse-leading-keywords body))
   (destructuring-bind (&key in description) opts)
   (let ((file (test-result-file name))))
   `(progn
      (save-suite ',name
                  :in ',in
                  :description ',description)
      ;; The file that holds the suite results.
      (overlord:file-target ,name ,file ()
        (run-suite-to-file ',name ,file))
      ;; The file that holds the suite's parent chain.
      (overlord:file-target
          ,(symbolicate name '.parents)
          ,(suite-parents-file name)
          ()
        (maybe-save-parents-file ',name))
      ;; A configuration that holds the dependencies.
      (overlord:defconfig ,(suite-deps-config name)
          ',(append
             (suite-parent-deps name)
             dependencies))
      ',name)))

(defmacro in-suite (name)
  `(eval-always
     (setf (current-suite)
           (find-test ',name))))

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
