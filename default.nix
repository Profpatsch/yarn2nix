{ nixpkgsPath ? <nixpkgs> }:
(import nixpkgsPath {
  overlays = [(pkgs: oldpkgs: {
    haskellPackages =
      let nix-lib = pkgs.callPackage ./nix-lib {};
      in oldpkgs.haskellPackages.override {
        overrides = oldpkgs.lib.composeExtensions
          (pkgs.callPackage ./nix-lib/old-version-dependencies.nix {})
          (self: super: {
            yarn2nix =
              let
                pkg = oldpkgs.haskell.lib.overrideCabal
                  (self.callPackage ./yarn2nix.nix {})
                  (old: {
                    prePatch = old.prePatch or "" + ''
                      ${oldpkgs.lib.getBin self.hpack}/bin/hpack
                    '';
                  });
              in oldpkgs.haskell.lib.overrideCabal pkg (old: {
                src = nix-lib.removePrefixes [ "nix-lib" "dist" "result" ] ./.;
              });
          });
      };
  })];

}).haskellPackages.yarn2nix
