{ pkgs ? import <nixpkgs> {}
, sourcesFile ? ./sources.json
}:

let
  sourcesDrv = pkgs.runCommandLocal "init-sources" {} ''
    ${pkgs.ndt}/bin/ndt -s ${sourcesFile} print > $out''
  ;
  sources = import sourcesDrv { inherit pkgs sourcesFile; } ;
in
  sources
