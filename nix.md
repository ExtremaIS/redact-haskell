The way that I currently use Nix to develop and test Haskell software does not
work with Cabal conditional compilation.  I do not have time to figure out an
alternate method at this time, so I am removing the Nix configuration for this
project.

I hope to get it working sometime in the future.  My goals/requirements are to
be able to do all of the following with all of the GHC versions listed in the
`tested-with` section of the `redact.cabal` file:

* build executables (for usage via Nix/NixOS)
* run the tests
* start a development shell
