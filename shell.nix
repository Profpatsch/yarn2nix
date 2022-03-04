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

  # easy-hls-nix source, as provided from the project.toml
  easy-hls-nixSrc = githubSrc {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    # 2021-06-26
    revision = "9d64543a015563942c954b89addc1108800ed134";
    sha256 = "1szq3g34dv22fqzil549mvpdd1865s64vqnfxj0l2aw9ha32jxyz";
  };

  easy-hls-nix = pkgs.callPackage easy-hls-nixSrc {
    ghcVersions = [ haskellPackages.ghc.version ];
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
      easy-hls-nix
    ];
  }
