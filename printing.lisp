(in-package #:proctor)

(defvar *indent* 0)

(defun print-test-result (result &aux (indent *indent*))
  "Print RESULT, a test result, to standard output."
  (match-of test-result result
    ((pass name)
     (format t "~&~v,0tTest ~a"
             indent
             name))
    ((failure test
              (trivia:lambda-list
               &key description
               &allow-other-keys))
     (format t "~&~v,0tTest ~a: FAIL~%~a"
             indent
             test
             description))
    ((suite-result test results)
     (mvlet* ((test-count pass-count fail-count
               (loop for test in results
                     sum 1 into tests
                     if (passed? test)
                       sum 1 into pass
                     else
                       sum 1 into fail
                     finally (return (values tests pass fail))))
              (passed? (= test-count pass-count)))
       (declare (ignore fail-count))
       (format t "~&~v,0tSuite ~a~@[: ~a~]"
               indent
               test
               (if passed? nil "FAIL"))
       ;; Print the individual results.
       (let ((*indent* (1+ indent)))
         (do-each (result results)
           (print-test-result result)))
       (unless passed?
         (format t "~&~v,0tSuite ~a: ~a/~a passed"
                 indent
                 test
                 pass-count
                 test-count))))))
