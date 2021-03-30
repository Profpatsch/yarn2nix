{ pkgs ? import ./nixpkgs-pinned.nix {} }:

let
  lib = pkgs.lib;

  licensesJson = pkgs.writeText "licenses.json"
    (builtins.toJSON (lib.filterAttrs (n: v: v ? spdxId) lib.licenses));

  haskellPackages = pkgs.haskellPackages.override {
    overrides =
      (self: super: {
        yarn-lock =
          let
            pkg = self.callPackage ./yarn-lock/yarn-lock.nix {};
          in pkgs.haskell.lib.overrideCabal pkg (old: {
            src = builtins.filterSource
              (path: type:
                 if lib.any (p: lib.hasPrefix (toString ./yarn-lock + "/" + p) path) [
                   "package.yaml"
                   "LICENSE"
                   "CHANGELOG.md"
                   "src"
                   "tests"
                 ]
                 then true
                 else false
              ) ./yarn-lock;
          });


        yarn2nix =
          let
            pkg = pkgs.haskell.lib.overrideCabal
              (self.callPackage ./yarn2nix/yarn2nix.nix {})
              (old: {
                prePatch = old.prePatch or "" + ''
                  ${lib.getBin self.hpack}/bin/hpack
                  # we depend on the git prefetcher
                  substituteInPlace \
                    src/Distribution/Nixpkgs/Nodejs/ResolveLockfile.hs \
                    --replace '"nix-prefetch-git"' \
                      '"${pkgs.nix-prefetch-git.override { git = pkgs.gitMinimal; }}/bin/nix-prefetch-git"'
                  sed -i '/license-data/a \ <> O.value "${licensesJson}"' \
                    src/Distribution/Nixpkgs/Nodejs/Cli.hs
                '';
              });
          in pkgs.haskell.lib.overrideCabal pkg (old: {
            src = builtins.filterSource
              (path: type:
                 if lib.any (p: lib.hasPrefix (toString ./yarn2nix + "/" + p) path) [
                   "package.yaml"
                   "LICENSE"
                   "src"
                   "NodePackageTool.hs"
                   "Main.hs"
                   "tests"
                 ]
                 then true
                 else false
              ) ./yarn2nix;
          });
      });
   };

   static = pkgs.haskell.lib.justStaticExecutables haskellPackages.yarn2nix;

   yarn2nix = pkgs.stdenv.mkDerivation {
     name = "yarn2nix";
     src = pkgs.nix-gitignore.gitignoreSource [ ".git/" ] ./yarn2nix;
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
            "Setup.hs"
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

     passthru.nixLib = import ./yarn2nix/nix-lib {
       inherit lib pkgs;
       inherit yarn2nix;
     };
   };

in yarn2nix
