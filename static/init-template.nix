{ pkgs ? import <nixpkgs> {}
, sourcesFile ? ./sources.json
}:

let
  sourcesDrv = pkgs.runCommand "init-sources" { buildInputs = [pkgs.ndt];} "ndt print > $out";
  sources = import "${sourcesDrv}" { inherit sourcesFile; } ;
in
  sources
