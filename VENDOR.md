# Vendored Dependencies

Some dependencies are vendored in order to support newer versions of GHC and
other dependencies than the upstream package supports.  This is generally done
because the vendored packages are not maintained.  Vendoring such packages
allows me to update the vendored copies yet still publish to Hackage.

## `HMock`

* [Hackage](https://hackage.haskell.org/package/HMock)
* [Repository](https://github.com/cdsmith/HMock)
* License: BSD-3
* Copyright: Copyright (c) 2021, Chris Smith

Vendored:

* [Repository](https://github.com/TravisCardwell/HMock/tree/vendored)
* Upstream commit:
  [`275fba1c`](https://github.com/cdsmith/HMock/tree/275fba1cf7d2c1e5022254fa0e90d495fd7cd8b8)
* Vendored commit:
  [`0bb7ba7f`](https://github.com/TravisCardwell/HMock/tree/0bb7ba7fc5bcd9d27049cd153bd313692b85aeb4)
    * The commit adding support for GHC 9.10 was merged, but Hackage has not
      been updated.
    * Bump `data-default` upper bound
    * Bump `extra` upper bound
* Files: `test/Test/HMock*`

## `explainable-predicates`

* [Hackage](https://hackage.haskell.org/package/explainable-predicates)
* [Repository](https://github.com/cdsmith/explainable-predicates)
* License: BSD-3
* Copyright: Copyright (c) 2021, Chris Smith

Vendored:

* [Repository](https://github.com/TravisCardwell/explainable-predicates/tree/vendored)
* Upstream commit:
  [`7eca242b`](https://github.com/cdsmith/explainable-predicates/tree/7eca242b75c778b65298a045daa278aa51a29983)
* Vendored commit: (same)
    * The commit adding support for GHC 9.10 was merged, but Hackage has not
      been updated.
* Files: `test/Test/Predicates*`
