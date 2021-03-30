let
  pkgs = import ./nixpkgs-pinned.nix {};
in
(pkgs.haskellPackages.override {
  overrides =
    (self: super: {
      my-pkg = let
        buildDepends = with self; [
          aeson
          ansi-wl-pprint
          async-pool
          either
          hnix
          http-client-tls
          monad-par
          neat-interpolation
          old-locale
          old-time
          protolude
          quickcheck-instances
          regex-tdfa
          tasty-hunit
          tasty-quickcheck
          tasty-th
        ];
        in super.mkDerivation {
          pname = "pkg-env";
          src = "/dev/null";
          version = "none";
          license = "none";
          inherit buildDepends;
          buildTools = [
            pkgs.hpack
            pkgs.cabal2nix
            self.ghcid
            pkgs.cabal-install
            (self.hoogleLocal {
              packages = buildDepends;
            })
          ];
        };
      });
}).my-pkg.env
