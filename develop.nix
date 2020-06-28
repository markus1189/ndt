with import <nixpkgs> { };
with haskell.lib;

dontHaddock (disableDeadCodeElimination (disableExecutableProfiling
  (disableLibraryProfiling (dontCheck (import ./default.nix { })))))
