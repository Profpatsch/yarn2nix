with import <nixpkgs> {};
(haskellPackages.override {
  overrides = self: super:
    let yarn-lock = super.mkDerivation rec {
          pname = "yarn-lock";
          version = "0.3.1";
          # src = ../../haskell/yarn-lock;
          src = fetchFromGitHub {
            owner = "Profpatsch";
            repo = "yarn-lock";
            rev = version;
            sha256 = "0qacc5x82kggzxgzn46irrqczyvfpdvhdar99ymk99fv7shzhgaf";
          };
          license = lib.licenses.mit;
          buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck ];
          buildTools = [ self.hpack ];
          preConfigure = ''hpack'';
        };
     in {
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
      };
}).my-pkg.env
