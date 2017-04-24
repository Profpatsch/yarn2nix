with import <nixpkgs> {};

let
  buildNodePackage = callPackage ./buildNodePackage.nix {};
  deps = import ./pkgs.nix { inherit buildNodePackage fetchurl; inherit (lib) fix; };

in deps
