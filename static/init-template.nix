{ pkgs ? import <nixpkgs> {}
, sourcesFile ? ./sources.json
}:

let
  sourcesDrv = pkgs.runCommand "init-sources" {} "${pkgs.ndt}/bin/ndt print > $out";
  sources = import sourcesDrv { inherit pkgs sourcesFile; } ;
in
  sources
