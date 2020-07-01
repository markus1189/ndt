{ pkgs
, sourcesFile
}:

let
  fetch_github = pkgs: spec:
    pkgs.fetchFromGitHub { inherit (spec) owner repo rev fetchSubmodules sha256; };

  fetch_url = pkgs: spec:
    pkgs.fetchurl { inherit (spec) url sha256 name; };

  # The actual fetching function.
  fetch = pkgs: name: spec:
    let specWithname = { name = "ndt-fetch-${name}"; } // spec; in
    if ! builtins.hasAttr "type" specWithname then
      abort "ERROR: ndt spec ${name} does not have a 'type' attribute"
    else if spec.type == "github" then fetch_github pkgs specWithname
    else if spec.type == "url" then fetch_url pkgs specWithname
    else
      abort "ERROR: ndt spec ${name} has unknown type ${builtins.toJSON spec.type}";

  mkSources = config:
    builtins.mapAttrs (
      name: spec:
        if builtins.hasAttr "outPath" spec
        then abort
          "The values in ${sourcesFile} should not have an 'outPath' attribute"
        else
          spec // { outPath = fetch pkgs name spec; }
    ) config.sources;

  # The "config" used by the fetchers
  mkConfig =
    { sources ? builtins.fromJSON (builtins.readFile sourcesFile)
    }: rec {
      # The sources, i.e. the attribute set of spec name to spec
      inherit sources;
    };
in
mkSources (mkConfig {}) // { __functor = _: settings: mkSources (mkConfig settings); } {}
