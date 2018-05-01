# proctor

Proctor is a Common Lisp test framework, designed for large,
resource-intensive test suites.

How is it different from other Common Lisp test frameworks? It treats
tests as build targets. You can very precisely specify the
dependencies of a test – from system versions to files to individual
Lisp bindings and OS environment variables – and the test, once
passed, will only be re-run if those dependencies change.

## API

The API of Proctor, as far as defining tests and test suites is
concerned, is a subset of [FiveAM][].

- `def-suite`
- `in-suite`
- `test`
- `is`
- `is-false`
- `is-true`
- `signals`
- `finishes`

For test suites that use only the basic features of FiveAM, switching
to Proctor may be as simple as changing a package to `:use` Proctor
instead of FiveAM.

Tests are run by name using `run` or `runq`:

    (run 'suite)
    (runq suite)

The test supplied to `run` is always run. Its dependencies, however,
are only run if they are out of date. Accordingly the argument to
`run` should usually be a suite, unless there is a specific test you
want to force.

Tests can also be run using `debug-test`. In this case, assertions
that fail will call `break`, with appropriate restarts, so you can
debug the problem.

Tests are run again if the definition of the test has changed.
Otherwise, tests are only run if one of their dependencies has
changed.

Tests that have failed are always considered out of date, and are
always re-run. The value of `*random-state*` at the time of the
initial failure is preserved, and used every time the test runs, until
the test passes.

Test suites can also have dependencies, but they have a diferent
meaning. The dependencies of a test suite become implicit dependencies
of all tests defined in that test suite. That means, if any of the
dependencies of the suite have changed, every test in that suite is
considered out of date.

Test suites can also be nested; when test suites are nested, the
contained test suite inherits all the suite-level dependencies of the
containing test suite.

## Examples

``` lisp
(in-package :proctor-user)

(def-suite my-tests
  (:depends-on
   ;; Re-run all tests if Quicklisp is upgraded.
   (:dist-version)
   ;; Re-run all tests if the major version of :my-system changes.
   (:system-version "my-system")))

(in-suite my-tests)

(test tricky-regression-test
  ;; Only rerun this test if the test data changes. Note that,
  ;; although the path is system relative, the test will still be
  ;; rerun if my-test-system is moved. (E.g., if you clone the system
  ;; for development).
  (:depends-on (:system-resource "my-system" "test-data/example.xml"))
  ...)

(test intensive-integration-test
  ;; Only rerun this test if the major version of the their-client has
  ;; changed.
  (:depends-on (:system-version "their-client"))
  ...)

(test simple-unit-test
  ;; This test should always be re-run.
  (:always t)
  ...)
```

Note that, while the forms beginning with keywords *look* declarative,
they are simply macros. For example, to depend on every file in a
directory:

``` lisp
(overlord:defconfig +test-files-directory+
    (asdf:system-relative-pathname "my-system" "test-data/"))

(overlord:define-target-config +test-files+
    (uiop:directory-files +test-files-directory+)
  ;; Depend on the binding.
  (:depends-on '+test-files-directory+)
  ;; Depend on the timestamp of the directory.
  (:depends-on +test-files-directory+))

(def-suite tests-from-files
  (:depends-on '+test-files+)
  (dolist (file +test-files+)
    (:depends-on file))
  ...)
```

Furthermore, these convenience macros have equivalent functions in the
`overlord` package, which you can call in helper functions.

``` lisp
(defun read-test-file-into-string (path)
  (let ((path (uiop:merge-pathnames* path +test-files-directory+)))
    (when (overlord:building?)
      (overlord:depends-on path))
    (alexandria:read-file-into-string path)))
```

## License

MIT

[Overlord]: https://github.com/TBRSS/overlord
[FiveAM]: https://common-lisp.net/project/fiveam/
