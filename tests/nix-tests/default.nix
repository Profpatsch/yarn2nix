{ nixpkgsPath ? <nixpkgs>
, nixLibPath ? ../../nix-lib
, yarn2nix ? import ../../. { inherit nixpkgsPath; }
}:
let
  pkgs = import nixpkgsPath {};
  nixLib = pkgs.callPackage nixLibPath {
    inherit yarn2nix;
  };

in nixLib.linkNodeDeps {
  name = "test";
  dependencies = [
    (nixLib.buildNodeDeps (pkgs.callPackage ./deps.nix {})).left-pad
  ];
}
