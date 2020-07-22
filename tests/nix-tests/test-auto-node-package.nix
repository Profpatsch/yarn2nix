{ pkgs, nixLib, yarn2nix }:

nixLib.buildNodePackage rec {
  src = ./top-pad;
  packageJson = src + /package.json;
  yarnLock = src + /yarn.lock;
}
