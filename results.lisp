(in-package #:proctor)

;;; How tests results are stored.

(defunion test-result
  "Type representing the results of a test (a test suite or a test
case)."
  (pass
   (test-name symbol))
  (failure
   (test-name symbol)
   (plist list))
  (suite-result
   (test-name symbol)
   (test-results list)))

(defun passed? (test-result)
  "Is TEST-RESULT a passing result?

A result is considered passing if it is an instance of `pass', or the
aggregated result of a test suite where every individual test is also
considered passing."
  (etypecase-of test-result test-result
    (pass t)
    (failure nil)
    (suite-result
     (every #'passed? (suite-result-test-results test-result)))))

(defun failed? (test-result)
  "Is TEST-RESULT a failing result?

A result is considered a failure if it did not pass."
  (not (passed? test-result)))
