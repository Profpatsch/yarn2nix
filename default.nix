with import <nixpkgs> {};

with lib;

let
  buildNodePackage = import ./buildNodePackage.nix {inherit linkNodeDeps stdenv;};
  # type: (NodePackageAttrs -> a) -> a
  # deps = build: (import ./pkgs.nix { inherit (lib) fix; inherit fetchurl; }) build;
  deps = build: (import ./hey.nix {
    inherit (lib) fix; inherit fetchurl fetchgit;
    buildNodePackage = build;
    });

  # type: ListOf File -> Path -> Drv
  filterSourcePrefixes = prfxs: src: builtins.filterSource
    (path: _: lib.any (srcF: lib.hasPrefix
                            ((builtins.toPath src) + "/" + srcF)
                            path)
                  prfxs)
    src;

  # TODO
  setupNodePackagePaths = haskellPackages.callPackage ./yarn2nix.nix {
    yarn-lock = callPackage ./yarn-lock.nix {} haskellPackages haskellPackages;
  };

  # type: String -> ListOf { name: String, drv : Drv } -> Drv
  linkNodeDeps = name: packageDeps:
    pkgs.runCommand (name + "-node_modules") {} ''
      mkdir -p $out/.bin
      ${lib.concatMapStringsSep "\n"
        (dep: ''
          echo BLABLUBB
          echo "linking ${dep.name}"
          echo "contents:"
          ls "${dep.drv}"
          ln -sT ${dep.drv} "$out/${dep.name}"
          ${setupNodePackagePaths}/bin/setup-node-package-paths bin --to=$out/.bin --package=$out/${dep.name}
        '')
        packageDeps}
    '';

in (callPackage ./package.nix {inherit filterSourcePrefixes buildNodePackage;})
     (deps (dep: { name = dep.name; drv = buildNodePackage dep; }))
