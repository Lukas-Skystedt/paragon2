
## Haddock documentation
Haddock is used to for documentation of modules. The command `cabal haddock` is used to generate the documentation.

## Tests
Tests are run using cabal's test driver. It uses both `parac` and `javac` from the commandline, requiring them to be in path. For `parac` this is done using `cabal install`.

There are three test suites, bad, good, and issues. The bad suite contains programs which are supposed to fail the parac compilation, these have an expected output associated with them specifying what error messages should be output. The good suite contains tests which should compile, both using `parac` and `javac`. The issues folder contains regression tests, created from issues from the original repo, where some are "bad" and some are "good".

The test suites are run using the command `cabal new-test --enable-tests <SUITE>`, where suite is bad, good, or issues.

