{ ghc
, nixpkgs
}:

let
  haskellPackages = nixpkgs.haskell.packages.${ghc};
  excludedFiles = ["ndt.nix" "default.nix" "shell.nix"];
  ndtSrc = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) excludedFiles) ) ./.;
  ndtDrv = haskellPackages.callCabal2nix "ndt" ndtSrc {};
in
  ndtDrv
