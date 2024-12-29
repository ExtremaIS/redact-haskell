# `redact-haskell` `0.6.0.0` Release Notes

Date
: 2024-12-30

## Overview

Redact is a utility for hiding secret text on the terminal.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/redact-haskell#readme>

## This Release

This release adds compatibility with newer releases of GHC and removes support
for versions of GHC that were released more than five years ago.  To do so,
dependencies `HMock` and `explainable-predicates` are vendored.

GHC versions 8.8.4 through 9.10.1 are supported.  Cabal version 3.0 through
3.14.1.0 are supported.  Support for GHC 9.12.1 is currently blocked but will
hopefully be added soon.

There are no changes to the API or CLI.

### Compatibility

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - redact-0.6.0.0
```

### Issues

There are no known issues at this time.
