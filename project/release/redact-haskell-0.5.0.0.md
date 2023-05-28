# `redact-haskell` `0.5.0.0` Release Notes

Date
: 2023-05-28

## Overview

Redact is a utility for hiding secret text on the terminal.

See the [README][] for details.

[README]: <https://github.com/ExtremaIS/redact-haskell#readme>

## This Release

This release adds compatibility with the latest version of the
`optparse-applicative` library.  Both lower and upper bounds of dependencies
are now tested in CI.  This release also includes changes to the project
management infrastructure.

There are no changes to the API or CLI.

### Compatibility

Build software:

| Software          | `redact` 0.4.0.0 | `redact` 0.5.0.0 |
| ----------------- | ---------------- | ---------------- |
| [GHC][]           | 8.2.2 ~ 9.2.1    | 8.2.2 ~ 9.6.2    |
| [cabal-install][] | 1.24 ~ 3.4       | 1.24 ~ 3.10      |

Library dependencies:

| Package           | `redact` 0.4.0.0  | `redact` 0.5.0.0    |
| ----------------- | ----------------- | ------------------- |
| [ansi-terminal][] | `>=0.8 && <0.12`  | `>=0.8.0.4 && <1.1` |
| [base][]          | `>=4.7 && <5`     | `>=4.10.1 && <4.19` |
| [text][]          | `>=1.2.3 && <2.1` | `>=1.2.3 && <2.1`   |

Executable dependencies:

| Package                  | `redact` 0.4.0.0  | `redact` 0.5.0.0    |
| ------------------------ | ----------------- | ------------------- |
| [ansi-wl-pprint][]       | `>=0.6 && <0.7`   | `>=0.6.8.2 && <1.1` |
| [directory][]            | `>=1.3 && <1.4`   | `>=1.3.0.2 && <1.4` |
| [optparse-applicative][] | `>=0.14 && <0.18` | `>=0.13 && <0.19`   |
| [prettyprinter][]        |                   | `>=1.7.1 && <1.8`   |

Test dependencies:

| Package                    | `redact` 0.4.0.0    | `redact` 0.5.0.0    |
| -------------------------- | ------------------- | ------------------- |
| [explainable-predicates][] | `>=0.1.2.1 && <0.2` | `>=0.1.2.1 && <0.2` |
| [HMock][]                  | `>=0.5.1 && <0.6`   | `>=0.5.1 && <0.6`   |
| [tasty][]                  | `>=1.0 && <1.5`     | `>=0.12 && <1.5`    |
| [tasty-hunit][]            | `>=0.10 && <0.11`   | `>=0.8 && <0.11`    |

To use this release with a Stackage snapshot that does not include it, add
the following to your `stack.yaml` configuration:

```yaml
extra-deps:
  - redact-0.5.0.0
```

[GHC]: <https://www.haskell.org/ghc/>
[cabal-install]: <https://hackage.haskell.org/package/cabal-install>
[ansi-terminal]: <https://hackage.haskell.org/package/ansi-terminal>
[base]: <https://hackage.haskell.org/package/base>
[text]: <https://hackage.haskell.org/package/text>
[ansi-wl-pprint]: <https://hackage.haskell.org/package/ansi-wl-pprint>
[directory]: <https://hackage.haskell.org/package/directory>
[optparse-applicative]: <https://hackage.haskell.org/package/optparse-applicative>
[explainable-predicates]: <https://hackage.haskell.org/package/explainable-predicates>
[HMock]: <https://hackage.haskell.org/package/HMock>
[tasty]: <https://hackage.haskell.org/package/tasty>
[tasty-hunit]: <https://hackage.haskell.org/package/tasty-hunit>

### Issues

There are no known issues at this time.
