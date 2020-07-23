{ nixpkgsPath ? ../../nixpkgs-pinned.nix
, nixLibPath ? ../../nix-lib
, yarn2nix ? import ../../. { inherit nixpkgsPath; }
}:
let
  pkgs = import nixpkgsPath {};
  nixLib = pkgs.callPackage nixLibPath {
    inherit yarn2nix;
  };

  inherit (import vendor/runTestsuite.nix { inherit pkgs; })
    runTestsuite
    it
    assertEq
    ;


in {
  testOverriding = import ./test-overriding.nix {
    inherit pkgs nixLib yarn2nix;
  };
}
