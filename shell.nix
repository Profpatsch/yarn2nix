{ pkgs ? import ./nixpkgs-pinned.nix {} }:

let
  haskellPackages = import ./haskell-pkgs.nix {
    inherit pkgs;
  };

  # fetch a github tarball, at evaluation time
  githubSrc = {
    owner,
    repo,
    revision,
    sha256
  }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${revision}.tar.gz";
      inherit sha256;
    };

in
  haskellPackages.shellFor {
    packages = hps: [
      hps.yarn2nix
      hps.yarn-lock
    ];
    withHoogle = true;
    buildInputs = [
      pkgs.ninja
      pkgs.cabal-install
      pkgs.hpack
      pkgs.ghcid
      haskellPackages.haskell-language-server
    ];
  }
