{ nixpkgsPath ? ../../nixpkgs-pinned.nix
, nixLibPath ? ../../nix-lib
, yarn2nix ? import ../../. { inherit nixpkgsPath; }
}:
let
  pkgs = import nixpkgsPath {};
  nixLib = pkgs.callPackage nixLibPath {
    inherit yarn2nix;
  };

  call = p: import p {
    inherit pkgs nixLib yarn2nix;
  };

in {
  testGeneration = call ./test-generation.nix;
  testOverriding = call ./test-overriding.nix;
  testGeneratedSanity = call ./test-generated-sanity.nix;
}
