# `redact-haskell` TODO

## Functionality

## Tests

## Compatibility

* Redact now builds using GHC 9.12.1 without issue using Cabal.  Stack 3.1.1
  has issues, but they should be resolved in Stack 3.3.1 (already released).
  When Stack 3.3.1 is added to GHCup, I will confirm, push, and make a Hackage
  revision.

## Documentation

## Project

* Decide what to do about `HMock`
    * Remove tests?
    * Switch to a different mock library?
    * Rewrite using an effects library?

* Arch `PKGBUILD`
* Windows executable
* Mac OS executable
