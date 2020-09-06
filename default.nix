{ pkgs ? import ./nixpkgs-pinned.nix {} }:

let
  lib = pkgs.lib;

  licensesJson = pkgs.writeText "licenses.json"
    (builtins.toJSON (lib.filterAttrs (n: v: v ? spdxId) lib.licenses));

  haskellPackages = pkgs.haskellPackages.override {
    overrides = lib.composeExtensions
      (pkgs.callPackage ./nix-lib/old-version-dependencies.nix {})
      (self: super: {
        yarn2nix =
          let
            pkg = pkgs.haskell.lib.overrideCabal
              (self.callPackage ./yarn2nix.nix {})
              (old: {
                prePatch = old.prePatch or "" + ''
                  ${lib.getBin self.hpack}/bin/hpack
                  # we depend on the git prefetcher
                  substituteInPlace \
                    src/Distribution/Nixpkgs/Nodejs/ResolveLockfile.hs \
                    --replace '"nix-prefetch-git"' \
                      '"${pkgs.nix-prefetch-git.override { git = pkgs.gitMinimal; }}/bin/nix-prefetch-git"'
                  sed -i '/license-data/a \ <> O.value "${licensesJson}" <> O.showDefault' \
                    src/Distribution/Nixpkgs/Nodejs/Cli.hs
                '';
              });
          in pkgs.haskell.lib.overrideCabal pkg (old: {
            src = builtins.filterSource
              (path: type:
                 if lib.any (p: lib.hasPrefix (toString ./. + "/" + p) path) [
                   "package.yaml"
                   "LICENSE"
                   "src"
                   "NodePackageTool.hs"
                   "Main.hs"
                   "Test.hs"
                   "tests"
                 ]
                 then true
                 else false
              ) ./.;
          });
      });
   };

   static = pkgs.haskell.lib.justStaticExecutables haskellPackages.yarn2nix;

   yarn2nix = pkgs.stdenv.mkDerivation {
     name = "yarn2nix";
     src = pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./.;
     outputs = [ "bin" "doc" "nixLib" "out" ];
     phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
     installPhase = ''
       install -D --target-directory=$bin/bin ${static}/bin/*
       mv "nix-lib/" $nixLib
       ${pkgs.skawarePackages.cleanPackaging.commonFileActions {
          noiseFiles = [
            "tests/*"
            "src/*"
            "Main.hs"
            "package.yaml"
            "yarn2nix.cabal"
            ".envrc"
            "shell.nix"
            "Repl.hs"
            "nixpkgs-pinned.nix"
            "yarn2nix.nix"
            "default.nix"
            "NodePackageTool.hs"
            ".gitignore"
            ".github"
          ];
          docFiles = [
            "README.md"
            "LICENSE"
            "CHANGELOG.md"
          ];
        }} "$doc/share/yarn2nix"
       ${pkgs.skawarePackages.cleanPackaging.checkForRemainingFiles}
     '';

     passthru.nixLib = import ./nix-lib {
       inherit lib pkgs;
       inherit yarn2nix;
     };
   };

in yarn2nix
