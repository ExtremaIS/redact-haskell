# `redact-haskell` TODO

## Functionality

## Tests

## Compatibility

* GHC 9.10.1 boot library
  [`base`](https://hackage.haskell.org/package/base)
  `4.20.0.0` blocked by
  [`HMock`](https://hackage.haskell.org/package/HMock)
  ([PR](https://github.com/cdsmith/HMock/pull/36)) and
  [`explainable-predicates`](https://hackage.haskell.org/package/explainable-predicates)
  ([PR](https://github.com/cdsmith/explainable-predicates/pull/21))
* [`tasty`](https://hackage.haskell.org/package/tasty)
  `1.5.1` is effectively disabled by constraint `base <0` made in revision 1,
  but it is not marked as deprecated.  See issue
  [#421](https://github.com/UnkindPartition/tasty/issues/421).

## Documentation

## Project

* Arch `PKGBUILD`
* Windows executable
* Mac OS executable
