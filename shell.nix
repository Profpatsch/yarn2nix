{ pkgs ? import ./nixpkgs-pinned.nix {} }:

let
  haskellPackages = import ./haskell-pkgs.nix {
    inherit pkgs;
  };
in
  haskellPackages.shellFor {
    packages = hps: [
      hps.yarn2nix
      hps.yarn-lock
    ];
    withHoogle = true;
    buildInputs = [
      pkgs.cabal-install
      pkgs.hpack
      pkgs.ghcid
    ];
  }
