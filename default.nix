{ ghc ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

let
  drv = import ./ndt.nix { inherit ghc nixpkgs; };
  modifiedDrv = with nixpkgs.haskell.lib;
    generateOptparseApplicativeCompletion "ndt" (
      disableExecutableProfiling (
        disableLibraryProfiling (
          justStaticExecutables drv)));
  drvWithInputs = modifiedDrv.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [
      nixpkgs.nix-prefetch-git
    ];
  });
  finalDrv = drvWithInputs;
in
  finalDrv
