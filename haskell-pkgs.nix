{ pkgs }:

let
  inherit (pkgs) lib;

  licensesJson = pkgs.writeText "licenses.json"
    (builtins.toJSON (lib.filterAttrs (n: v: v ? spdxId) lib.licenses));

  minimalHaskellSource = root: extra:
    builtins.path {
      path = root;
      name = "${builtins.baseNameOf root}-source";
      filter = path: type:
        lib.any (p: lib.hasPrefix (toString root + "/" + p) path) ([
          "package.yaml"
          "LICENSE"
          "CHANGELOG.md"
          "src"
          "tests"
        ] ++ extra);
    };
in

pkgs.haskellPackages.override {
  overrides =
    (self: super: {
      yarn-lock =
        let
          pkg = self.callPackage ./yarn-lock/yarn-lock.nix {};
        in pkgs.haskell.lib.overrideCabal pkg (old: {
          version = "git";
          src = minimalHaskellSource ./yarn-lock [];
        });

      yarn2nix =
        let
          pkg = self.callPackage ./yarn2nix/yarn2nix.nix {};
        in pkgs.haskell.lib.overrideCabal pkg (old: {
          version = "git";
          src = minimalHaskellSource ./yarn2nix [
            "Main.hs"
            "NodePackageTool.hs"
          ];

          prePatch = ''
            # we depend on the git prefetcher
            substituteInPlace \
              src/Distribution/Nixpkgs/Nodejs/ResolveLockfile.hs \
              --replace '"nix-prefetch-git"' \
                '"${pkgs.nix-prefetch-git.override { git = pkgs.gitMinimal; }}/bin/nix-prefetch-git"'
            sed -i '/license-data/a \ <> O.value "${licensesJson}"' \
              src/Distribution/Nixpkgs/Nodejs/Cli.hs
          '' + old.prePatch or "";
        });
      });
   }
