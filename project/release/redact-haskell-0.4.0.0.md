# `redact-haskell` `0.4.0.0` Release Notes

Date
: 2022-03-02

## Overview

Redact is a utility for hiding secret text on the terminal.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/redact-haskell#readme>

## This Release

This release of Redact is almost a complete rewrite.  It is now implemented in
a way that allows for extensive testing using mocks.  The `redact`
command-line utility also has significant changes.

### `MonadTerminal`, `MonadHandle`, and Mock Tests

The project was redesigned to use a library.  The API uses (`MonadTerminal`
and `MonadHandle`) type classes instead of `IO` directly.  This allows much
more of the implementation to be tested, using the [HMock][] library.

[HMock]: <https://hackage.haskell.org/package/HMock>

### `--lenient` Flag

In this release of Redact, the program terminates if there is a parsing error.
A `--lenient` flag is added, which causes the program to ignore parsing
errors, like the behavior in previous versions.

### Exception Handling

If the program has an error, an exception handler now resets the terminal.
This prevents the terminal from remaining in redact mode in cases when an
error is raised while in redact mode.

### Dependency Versions

The following dependency version upper bound has been bumped to support the
latest version.

* [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)

### Compatibility

Redact is currently tested with [GHC 8.2.2][] through [GHC 9.2.1][].  The
`.cabal` file uses Cabal version 1.24 (included with GHC 8.2.2), so it should
build fine on relatively old Haskell installations as well as current
installations.

[GHC 8.2.2]: <https://www.haskell.org/ghc/download_ghc_8_2_2.html>
[GHC 9.2.1]: <https://www.haskell.org/ghc/download_ghc_9_2_1.html>

### Issues

There are no known issues at this time.
