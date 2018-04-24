;;;; proctor.lisp

(in-package #:proctor)

(def proctor-dir
  (path-join
   (user-homedir-pathname)
   (make-pathname
    :directory `(:relative
                 "proctor"
                 ,(uiop:implementation-identifier))))
  "Directory where Proctor test results are cached.")

(defconst .results "results"
  "Extension for test result files.")

(defconst .tests "tests"
  "Extension for the files that hold the list of tests associated with
  a test suite.")

(defconst .parents "parents"
  "Extension for the file that holds the list of parents of a suite.")

(-> test-name (t) symbol)
(defgeneric test-name (test)
  (:documentation "The name (a symbol) of TEST.")
  (:method ((test null))
    nil)
  (:method ((test symbol))
    test))

(defgeneric run-test (test)
  (:documentation "Run TEST.")
  (:method ((test symbol))
    (run-test (find-test test))))

(defgeneric run-test-to-string (test)
  (:documentation "Run TEST, writing the results to a string.")
  (:method ((test symbol))
    (run-test-to-string (find-test test))))

(defgeneric run-suite-to-file (suite file)
  (:documentation "Run SUITE, a test suite, (conditionally) writing
  the aggregated results into a file.")
  (:method ((suite symbol) file)
    (run-suite-to-file (find-suite suite) file)))

(defun get-test-result (test)
  "Run TEST and get the test result."
  (~> test
      build-result-file
      read-test-result-from-file))

(defun read-test-result-from-file (file)
  "Read a single object, which must be a test result, from FILE."
  (assure test-result
    (read-object-from-file file)))

(defun build-result-file (test)
  "Run TEST, if necessary, and write the result into a file.
Return the file."
  (lret ((target (test-result-file test)))
    (prog1 (overlord:build target)
      (unless (file-exists-p target)
        (error "There is no such test as ~a."
               (test-name test))))))

(defvar *test-output*
  (make-synonym-stream '*standard-output*)
  "Stream to write to when test assertions fail.")
(declaim (type output-stream *test-output*))

(defun run-test-to-file (test file)
  "Re-run TEST, possibly (but not necessarily) updating FILE as a
result."
  (let* ((result (run-test-to-result test))
         (string
           (with-standard-io-syntax
             (write-to-string result :readably t))))
    ;; If the test failed, it should always be redone.
    (when (failed? result)
      (overlord:redo-always))
    (overlord:write-file-if-changed string file)))

(defun random-state-for-test (test)
  (assure random-state
    (let ((file (test-result-file test)))
      (or (when (file-exists-p file)
            (let ((object (read-object-from-file file)))
              (when (typep object 'failure)
                (getf (failure-plist object) :random-state))))
          (make-random-state nil)))))

(defun run-test-to-result (test)
  "Run TEST and return a test-result object."
  (let ((random-state (random-state-for-test test)))
    (let ((string (run-test-to-string test)))
      (assure test-result
        (if (emptyp string) (pass test)
            (failure (test-name test)
                     (list :random-state random-state
                           :description string)))))))

(defclass abstract-test ()
  ((name
    :type symbol
    :initarg :name
    :reader test-name)
   (docstring
    :type (or string null)
    :initarg :documentation
    :initarg :description
    :reader test-documentation)
   (parent
    :initform nil
    :reader test-parent))
  (:default-initargs
   :name (required-argument :name)
   :documentation "A test that can be run, which could be an
   individual test case, or a suite of tests, some or all of which
   could also be suites."))

(defmethod print-object ((self abstract-test) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s" (test-name self))))

(defvar *tests* (make-hash-table)
  "Table that associates names with tests.")

(defgeneric find-test (name)
  (:documentation "Find the test named NAME.
If there is no such test, return nil.")
  (:method ((test abstract-test))
    test)
  (:method ((name symbol))
    (synchronized ('*tests*)
      (values
       (gethash (assure symbol name)
                *tests*)))))

(defun (setf find-test) (test name)
  "Save TEST under NAME."
  (synchronized ('*tests*)
    (setf (gethash (assure symbol name) *tests*)
          (assure abstract-test test))))

(defun find-suite (name)
  "Find a test named NAME, which must be a suite."
  (assure suite
    (find-test name)))

(defvar *suite* nil
  "The current suite, or nil if no suite is current.")
(declaim (type symbol *suite*))

(defplace current-suite ()
  *suite*
  "Get (set) the current suite.")

(defclass suite (abstract-test)
  ((deps-fn :initarg :deps-fn
            :type function
            :reader suite-deps-fn))
  (:documentation "A test suite."))

(defgeneric run-suite-deps (suite)
  (:method ((suite symbol))
    (run-suite-deps (find-suite suite)))
  (:method ((suite suite))
    (funcall (suite-deps-fn suite))))

(defvar *suite->tests* (make-hash-table)
  "Table that maps suites (by name) to their associated tests.")

(defvar *test->suite* (make-hash-table)
  "Table that maps tests (by name) to their associated suite.
A test can only belong to one suite at a time.")

(defun suite-tests (suite)
  "Get the tests associated with SUITE."
  (values (gethash (test-name suite) *suite->tests*)))

(defun test-suite (test)
  "Get the (unique) suite that TEST belongs to."
  (values (gethash (test-name test) *test->suite*)))

(defun test-parents (test)
  (nlet rec ((test test)
             (acc nil))
    (let ((parent (test-suite test)))
      (if (no parent)
          (nreverse acc)
          (rec parent
               (cons parent acc))))))

(defun maybe-save-parents-file (suite)
  (let* ((file (suite-parents-file suite))
         (parents (test-parents suite))
         (string
           (with-standard-io-syntax
             (prin1-to-string parents))))
    (overlord:write-file-if-changed string file)))

(defun suite-deps-config (name)
  (symbolicate name '.deps))

(defun suite-deps (name)
  (bound-value (suite-deps-config name)))

(defun suite-parent-deps (name)
  (mappend #'suite-deps (test-parents name)))

(defun depend-on-suite-parents (suite)
  (maybe-save-parents-file suite)
  (overlord:depends-on (suite-parents-file suite)))

(defun unquote (form)
  (trivia:match form
    ((list 'quote x) x)
    (otherwise form)))

(defmacro depend-on-suite-deps (suite)
  (let* ((suite (unquote suite))
         (config (suite-deps-config suite)))
    `(progn
       ;; Use `use' instead of `depends-on' in case the parent suite
       ;; isn't defined yet.
       (overlord:use ',config)
       (run-suite-deps ',suite))))

(defun add-test-to-suite (test suite)
  "Add TEST to SUITE.
If TEST already belongs to a suite, it is re-assigned."
  (let ((test (test-name test))
        (suite (test-name suite)))
    (synchronized ()
      ;; Remove any old mapping.
      (when-let (old (pophash test *test->suite*))
        (removef (gethash old *suite->tests*) test))
      ;; Add the new mapping.
      (when suite
        (setf (gethash test *test->suite*) suite)
        (pushnew test (gethash suite *suite->tests*)))))
  (values))

(defun save-suite (name &key in description deps-fn)
  "Register a suite."
  (add-test-to-suite name in)
  (setf (find-test name)
        (make 'suite
              :name name
              :description description
              :deps-fn deps-fn)))

(defmethod run-suite-to-file ((suite suite) file)
  "Run SUITE and update FILE accordingly.
This also updates, and registers a dependency on, a second file that
holds the dependencies of SUITE."
  (maybe-update-suite-tests-file suite)
  (overlord:depends-on (suite-tests-file suite))
  (let* ((tests (suite-tests suite))
         (result-files (mapcar #'test-result-file tests)))
    (overlord:pdepends-on-all result-files)
    (maybe-save-suite-results suite file result-files)))

(defun maybe-update-suite-tests-file (suite)
  "Update the file that stores the dependencies of SUITE, if
necessary."
  (let* ((tests (suite-tests suite))
         (tests (stable-sort-new tests #'string<))
         (string
           ;; NB This doesn't need to be readable, it just needs to be
           ;; consistent.
           (with-output-to-string (s)
             (with-standard-io-syntax
               (do-each (test tests)
                 (format s "~s~%" test)))))
         (file (suite-tests-file suite)))
    (overlord:write-file-if-changed string file)))

(defun maybe-save-suite-results (suite file result-files)
  "Update the file that stores the aggregated results of SUITE, if
necessary."
  (let* ((forms (mapcar #'read-test-result-from-file result-files))
         (results (suite-result (test-name suite) forms))
         (pass? (passed? results))
         (string
           (with-standard-io-syntax
             (write-to-string results :readably t))))
    (unless pass?
      (overlord:redo-always))
    (overlord:write-file-if-changed string file)))

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
   :package *package*
   :function (required-argument :function))
  (:documentation "A single test case."))

(defun make-test (name fn)
  "Constructor for test cases."
  (make 'test :name name :function fn))

(defun save-test (name thunk &key in)
  "Register a test case."
  (add-test-to-suite name in)
  (setf (find-test name)
        (make-test name thunk)))

(defmethod run-test-to-string ((test test))
  (with-slots (package name function) test
    (with-output-to-string (*test-output*)
      (run-test test))))

(defmethod run-test ((test test))
  (with-slots (package function) test
    (let ((*package* package))
      (nlet rec ()
        (restart-case
            (funcall function)
          (retry ()
            :report "Run the test again."
            (rec)))))))

(defun test-related-file (test ext)
  "Get a file suitable for storing values related to TEST.
Different values can be stored in separate files with different
extensions."
  (let* ((test (test-name test))
         (package (symbol-package test))

         (package-name
           (package-name package))
         (symbol-name
           (symbol-name test))

         (package-name
           (escape-pathname-component package-name))
         (symbol-name
           (escape-pathname-component symbol-name)))
    (ensure-directories-exist
     (path-join
      proctor-dir
      (make-pathname
       :directory `(:relative
                    ,package-name)
       :name symbol-name
       :type ext)))))

(defun escape-pathname-component (string)
  (~>> (assure string string)
       (substitute #\_ #\/)
       (substitute #\_ #\.)))

(defun test-result-file (test)
  "The file to store the results of TEST in."
  (test-related-file test .results))

(defun suite-tests-file (suite)
  "The file to store the dependencies of SUITE in."
  (test-related-file suite .tests))

(defun suite-parents-file (suite)
  (test-related-file suite .parents))

(defun read-object-from-file (file)
  "Read (with `read') a single object from FILE.
If FILE contains other objects, they are ignored."
  (with-input-from-file (in file :element-type 'character)
    (with-standard-io-syntax
      (read in))))

(defun print-backtrace-to-string (&optional error)
  "Print a backtrace to a string, using UIOP for portability."
  (with-output-to-string (s)
    (uiop:print-backtrace
     :stream s
     :condition error)))
