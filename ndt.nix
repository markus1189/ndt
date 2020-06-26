{ ghc
, nixpkgs
}:

let
  haskellPackages = nixpkgs.haskell.packages.${ghc};
  ndtDrv = haskellPackages.callCabal2nix "ndt" ./. {};
in
  nixpkgs.haskell.lib.shellAware ndtDrv
