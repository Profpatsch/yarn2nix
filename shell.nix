let
  pkgs = import ./nixpkgs-pinned.nix {};
in
(pkgs.haskellPackages.override {
  overrides =
    (self: super: {
      my-pkg = let
        buildDepends = with self; [
          protolude
          hnix
          aeson
          async-pool
          ansi-wl-pprint
          regex-tdfa
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
