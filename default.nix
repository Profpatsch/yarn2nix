with import <nixpkgs> {};

with lib;

let
  buildNodePackage = import ./buildNodePackage.nix {inherit linkNodeDeps stdenv jq;};
  # type: (NodePackageAttrs -> a) -> a
  deps = build: (import ./pkgs.nix { inherit (lib) fix; inherit fetchurl; }) build;

  # type: ListOf File -> Path -> Drv
  filterSourcePrefixes = prfxs: src: builtins.filterSource
    (path: _: lib.any (srcF: lib.hasPrefix
                            ((builtins.toPath src) + "/" + srcF)
                            path)
                  prfxs)
    src;

  # type: String -> ListOf { name: String, drv : Drv } -> Drv
  linkNodeDeps = name: packageDeps:
    pkgs.runCommand (name + "-node_modules") {} ''
      mkdir -p $out
      ${lib.concatMapStringsSep "\n"
        (dep: ''
          echo "linking ${dep.name}"
          ln -sT ${dep.drv} "$out/${dep.name}"
        '')
        packageDeps}
    '';

in (callPackage ./package.nix {inherit filterSourcePrefixes buildNodePackage;})
     (deps (dep: { name = dep.name; drv = buildNodePackage dep; }))
