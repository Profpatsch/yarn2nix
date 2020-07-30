{ nixpkgsPath ? ./nixpkgs-pinned.nix }:

let
  pkgs = import nixpkgsPath {};
  nix-lib = pkgs.callPackage ./nix-lib {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = pkgs.lib.composeExtensions
      (pkgs.callPackage ./nix-lib/old-version-dependencies.nix {})
      (self: super: {
        yarn2nix =
          let
            pkg = pkgs.haskell.lib.overrideCabal
              (self.callPackage ./yarn2nix.nix {})
              (old: {
                prePatch = old.prePatch or "" + ''
                  ${pkgs.lib.getBin self.hpack}/bin/hpack
                  # we depend on the git prefetcher
                  substituteInPlace \
                    src/Distribution/Nixpkgs/Nodejs/ResolveLockfile.hs \
                    --replace '"nix-prefetch-git"' \
                      '"${pkgs.nix-prefetch-scripts}/bin/nix-prefetch-git"'
                '';
              });
          in pkgs.haskell.lib.overrideCabal pkg (old: {
            src = nix-lib.removePrefixes [
              "nix-lib"
              "dist"
              "result"
              "tests/nix-tests"
              ".git"
              ".github"
              "default.nix"
            ] ./.;
          });
      });
   };

in haskellPackages.yarn2nix
