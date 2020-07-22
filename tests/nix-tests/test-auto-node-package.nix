{ pkgs, nixLib, yarn2nix }:

nixLib.buildNodePackage rec {
  key = {
    scope = "";
    name = "top-pad";
  };

  version = "0.1.0";

  src = ./top-pad;
  packageJson = src + /package.json;
  yarnLock = src + /yarn.lock;
}
