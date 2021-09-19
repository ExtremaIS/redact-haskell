# Nix configuration for testing redact against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  redact-ghc-822 = import ./default.nix { compiler = "ghc822"; };
  redact-ghc-844 = import ./default.nix { compiler = "ghc844"; };
  redact-ghc-865 = import ./default.nix { compiler = "ghc865"; };
  redact-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  redact-ghc-8107 = import ./default.nix { compiler = "ghc8107"; };
  redact-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
