{ ghc
, nixpkgs
}:

let
  haskellPackages = nixpkgs.haskell.packages.${ghc}.override {
    overrides = self: super: {
      rio = self.callHackageDirect {
        pkg = "rio";
        ver = "0.1.17.0";
        sha256 = "07qxvw168vqv9ma9x4v1g4aiddb6v1cdhl9wa0zbx9980xzpqx81";
      } {};
    };
  };
  excludedFiles = ["ndt.nix" "default.nix" "shell.nix"];
  ndtSrc = builtins.filterSource (path: type: builtins.elem (baseNameOf path) excludedFiles ) ./.;
  ndtDrv = haskellPackages.callCabal2nix "ndt" ndtSrc {};
in
  ndtDrv
