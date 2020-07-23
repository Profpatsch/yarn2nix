{ nixpkgsPath ? ../../nixpkgs-pinned.nix
, nixLibPath ? ../../nix-lib
, yarn2nix ? import ../../. { inherit nixpkgsPath; }
}:
let
  pkgs = import nixpkgsPath {};
  nixLib = pkgs.callPackage nixLibPath {
    inherit yarn2nix;
  };


in {
  testOverriding = import ./test-overriding.nix {
    inherit pkgs nixLib yarn2nix;
  };
}
