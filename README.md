# proctor

Proctor is a Common Lisp test framework, designed for large, resource-intensive test suites.

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

Dependencies are defined using the syntax of [Overlord][].

## Examples

## License

MIT


[Overlord]: https://github.com/TBRSS/overlord
[FiveAM]: https://common-lisp.net/project/fiveam/
