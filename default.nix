(import <nixpkgs> {
  overlays = [(pkgs: oldpkgs: {
    haskellPackages =
      let nix-lib = pkgs.callPackage ./nix-lib {};
      in oldpkgs.haskellPackages.override {
        overrides = self: super: {
          inherit (pkgs.callPackage ./nix-lib/yarn-lock.nix {} self super) yarn-lock;
          yarn2nix =
            let pkg = self.callPackage ./yarn2nix.nix {};
            in oldpkgs.haskell.lib.overrideCabal pkg (old: {
              src = nix-lib.removePrefixes [ "nix-lib" "dist" "result" ] ./.;
            });
        };
      };
  })];

}).haskellPackages.yarn2nix
