with import <nixpkgs> {};

let
  inherit (import ./nix-lib/default.nix { inherit pkgs lib; })
    buildNodeDeps callTemplate;

in callTemplate ./package.nix (buildNodeDeps ./hey2.nix)
