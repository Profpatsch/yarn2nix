with import <nixpkgs> {};
(haskellPackages.override {
  overrides = lib.composeExtensions
    (pkgs.callPackage ./nix-lib/old-version-dependencies.nix {})
    (self: super: {
      my-pkg = let
        buildDepends = with self; [
          protolude
          yarn-lock
          hnix
          hpack
          aeson
          async-pool
          ansi-wl-pprint
          regex-tdfa
          regex-tdfa-text
          neat-interpolation
          tasty-th
          tasty-quickcheck
          tasty-hunit
          http-client-tls
          monad-par
        ];
        in super.mkDerivation {
          pname = "pkg-env";
          src = "/dev/null";
          version = "none";
          license = "none";
          inherit buildDepends;
          buildTools = with self; [
            ghcid
            cabal-install
            (hoogleLocal {
              packages = buildDepends;
            })
          ];
        };
      });
}).my-pkg.env
