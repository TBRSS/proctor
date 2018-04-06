(in-package #:proctor)

(defvar *indent* 0)

(defun print-test-result (result &aux (indent *indent*))
  "Print RESULT, a test result."
  (match-of test-result result
    ((pass name)
     (format t "~&~v,0tTest ~a"
             indent
             name))
    ((failure test
              (trivia:lambda-list
               &key description
               &allow-other-keys))
     (format t "~&~v,0tTest ~a: FAIL.~%~a"
             indent
             test
             description))
    ((suite-result test results)
     (format t "~&~v,0tSuite ~a~@[: ~a~]."
             indent
             test
             (eif (every #'passed? results) nil "FAIL"))
     (let ((*indent* (1+ indent)))
       (do-each (result results)
         (print-test-result result))))))
