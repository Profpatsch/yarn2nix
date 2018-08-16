{ nixpkgsPath    ? ./nixpkgs-pinned.nix,
  config         ? {},
  overlaysBefore ? [],
  overlaysAfter  ? []
}:

(import nixpkgsPath {
  inherit config;
  overlays = overlaysBefore ++ [
    (self: super: {
      haskellPackages = (
        let nix-lib = self.callPackage ./nix-lib {};
        in super.haskellPackages.override {
          overrides = super.lib.composeExtensions
            (self.callPackage ./nix-lib/old-version-dependencies.nix {})
            (hself: hsuper: {
              yarn2nix = (
                let
                  pkg = super.haskell.lib.overrideCabal
                    (hself.callPackage ./yarn2nix.nix {})
                    (old: {
                      prePatch = old.prePatch or "" + ''
                        ${super.lib.getBin hself.hpack}/bin/hpack
                        # we depend on the git prefetcher
                        substituteInPlace \
                          src/Distribution/Nixpkgs/Nodejs/ResolveLockfile.hs \
                          --replace '"nix-prefetch-git"' \
                            '"${self.nix-prefetch-scripts}/bin/nix-prefetch-git"'
                      '';
                    });
                in super.haskell.lib.overrideCabal pkg (old: {
                  src = nix-lib.removePrefixes
                    [ "nix-lib" "dist" "result" "tests/nix-tests" ] ./.;
                }));
            });
        }
      );
    })
  ] ++ overlaysAfter;
}).haskellPackages.yarn2nix
