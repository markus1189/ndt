with import <nixpkgs> { };
with haskell.lib;

dontHaddock (
  disableDeadCodeElimination (
    disableExecutableProfiling (
      disableLibraryProfiling (
        import ./default.nix {}))))
