{ pkgs ? import ./nixpkgs-pinned.nix {} }:

let
  lib = pkgs.lib;

  haskellPackages = import ./haskell-pkgs.nix {
    inherit pkgs;
  };

  static = pkgs.haskell.lib.justStaticExecutables haskellPackages.yarn2nix;

  yarn2nix = pkgs.stdenv.mkDerivation {
    name = "yarn2nix";
    src = "${pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./.}/yarn2nix";
    outputs = [ "bin" "doc" "out" ];
    phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
    installPhase = ''
      install -D --target-directory=$bin/bin ${static}/bin/*
      ${pkgs.skawarePackages.cleanPackaging.commonFileActions {
         noiseFiles = [
           "tests/*"
           "src/*"
           "Main.hs"
           "Setup.hs"
           "package.yaml"
           "yarn2nix.cabal"
           ".envrc"
           "Repl.hs"
           "yarn2nix.nix"
           "NodePackageTool.hs"
           "nix-lib"
         ];
         docFiles = [
           "README.md"
           "LICENSE"
           "CHANGELOG.md"
         ];
       }} "$doc/share/yarn2nix"
      ${pkgs.skawarePackages.cleanPackaging.checkForRemainingFiles}
    '';

    passthru.nixLib = import ./yarn2nix/nix-lib {
      inherit lib pkgs;
      inherit yarn2nix;
    };
  };

in yarn2nix
