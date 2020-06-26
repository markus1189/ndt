{ ghc ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

import ./ndt.nix { inherit ghc nixpkgs; }
