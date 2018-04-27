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

Tests are run by name using `run` or `runq`:

    (run 'suite)
    (runq suite)

Tests are run again if the definition of the test has changed.
Otherwise, tests are only run if one of their dependencies has
changed.

Test suites can also have dependencies, but they work differently. The
dependencies of a test suite become dependencies of all tests defined
in that test suite. That means, if any of the dependencies of the
suite have changed, every test in that suite is considered out of
date.

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
```

## License

MIT

[Overlord]: https://github.com/TBRSS/overlord
[FiveAM]: https://common-lisp.net/project/fiveam/
