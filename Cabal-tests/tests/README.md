Unit tests
==========

Ordinary unit tests.  If you're looking for the package tests,
they live in cabal-testsuite now.

If running these tests to update the expected output, do so from the project
root.

```
$ cabal run Cabal-tests:parser-tests -- --accept
$ cabal run Cabal-tests:check-tests -- --accept
```