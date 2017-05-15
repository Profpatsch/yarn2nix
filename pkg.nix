with import <nixpkgs> {};
(haskellPackages.override {
  overrides = self: super:
    let yarn-lock = super.mkDerivation {
          pname = "yarn-lock";
          version = "0.1.0";
          src = ../../haskell/yarn-lock;
          # src = fetchFromGitHub {
          #   owner = "Profpatsch";
          #   repo = "yarn-lock";
          #   rev = "0.1.0";
          #   sha256 = "0jrixxg4h6wfmn57xgklmhp1cprq1jn4mzl9001gdl6kpi2cbkg4";
          # };
          license = lib.licenses.mit;
          buildDepends = with self; [ megaparsec protolude regex-tdfa regex-tdfa-text ansi-wl-pprint tasty-hunit tasty-th ];
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
}).my-pkg
