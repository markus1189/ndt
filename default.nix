{ ghc ? "ghc883"
, nixpkgs ? import <nixpkgs> {}
}:

let
  drv = import ./ndt.nix { inherit ghc nixpkgs; };
  drvOnlyExecutables = nixpkgs.haskell.lib.justStaticExecutables drv;
  drvWithCompletion = drvOnlyExecutables.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.installShellFiles ];
    postInstall = ''
      $out/bin/ndt --bash-completion-script $out/bin/ndt > ndt.bash
      installShellCompletion --bash --name ndt.bash ndt.bash

      $out/bin/ndt --zsh-completion-script $out/bin/ndt > ndt.zsh
      installShellCompletion --zsh --name ndt.zsh ndt.zsh

      $out/bin/ndt --fish-completion-script $out/bin/ndt > ndt.fish
      installShellCompletion --fish --name ndt.fish ndt.fish
    '';
  });
in
  drvWithCompletion
