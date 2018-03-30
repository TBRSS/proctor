(in-package #:proctor)

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
