{ yarn2nix, nixLib, pkgs }:

# TODO use built-local nix to instantiate the files.
let
  yarnLack = pkgs.writeText "yarn.lock" ''
    left-pad@1.2.3: 
  '';
in {}
