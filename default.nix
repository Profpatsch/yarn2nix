{ nixpkgsPath ? builtins.fetchTarball {
    # nixpkgs unstable 2018-05-31
    url = "https://github.com/NixOS/nixpkgs/archive/5b468ea6b1d8d243847a05bdf5603e8abdfd7b4e.tar.gz";
    sha256 = "0yj31a11h0mj9d5jyz95ynh5791nd0y1xzqk7ba3qrikljqg3lvb";
  }
}:
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
                      # we depend on the git prefetcher
                      substituteInPlace \
                        src/Distribution/Nixpkgs/Nodejs/ResolveLockfile.hs \
                        --replace '"nix-prefetch-git"' \
                          '"${pkgs.nix-prefetch-scripts}/bin/nix-prefetch-git"'
                    '';
                  });
              in oldpkgs.haskell.lib.overrideCabal pkg (old: {
                src = nix-lib.removePrefixes
                  [ "nix-lib" "dist" "result" ] ./.;
              });
          });
      };
  })];

}).haskellPackages.yarn2nix
