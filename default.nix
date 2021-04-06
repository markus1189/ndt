{ ghc
, nixpkgs ? import <nixpkgs> {}
}:

let
  drv = import ./ndt.nix { inherit ghc nixpkgs; };
  modifiedDrv = with nixpkgs.haskell.lib;
    disableExecutableProfiling (
      disableLibraryProfiling (
        justStaticExecutables drv));
  drvWithInputs = modifiedDrv.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [
      nixpkgs.nix-prefetch-git
    ];

    nativeBuildInputs = old.nativeBuildInputs ++ [
      nixpkgs.installShellFiles
      nixpkgs.makeWrapper
    ];

    postInstall = ''
      $out/bin/ndt --bash-completion-script $out/bin/ndt > ndt.bash
      installShellCompletion --bash --name ndt.bash ndt.bash

      $out/bin/ndt --zsh-completion-script $out/bin/ndt > ndt.zsh
      installShellCompletion --zsh --name ndt.zsh ndt.zsh

      $out/bin/ndt --fish-completion-script $out/bin/ndt > ndt.fish
      installShellCompletion --fish --name ndt.fish ndt.fish

      wrapProgram $out/bin/ndt --prefix PATH : ${nixpkgs.lib.makeBinPath [
        nixpkgs.nix-prefetch-git
      ]}
    '';
  });
  finalDrv = drvWithInputs;
in
  finalDrv
