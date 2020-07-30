{ nixpkgsPath ? ./nixpkgs-pinned.nix }:

let
  pkgs = import nixpkgsPath {};
  lib = pkgs.lib;
  nix-lib = pkgs.callPackage ./nix-lib {};
  exactSource = import ./nix-lib/exact-source.nix;

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
                      '"${pkgs.nix-prefetch-scripts}/bin/nix-prefetch-git"'
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

in haskellPackages.yarn2nix
