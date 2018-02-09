((import <nixpkgs> {}).haskellPackages.override {
  overrides = self: super: {
        megaparsec =
          super.mkDerivation {
            pname = "megaparsec";
            version = "5.3.1";
            sha256 = "06myn8l6jcbd494i3wr6q27npbbxd6c2gfkd2jdzwbjqjqbpv0j8";
            libraryHaskellDepends = with self; [
              base bytestring containers deepseq exceptions mtl
              parser-combinators scientific text transformers
            ];
            testHaskellDepends = with self; [
              base bytestring containers hspec hspec-expectations mtl QuickCheck
              scientific text transformers
            ];
            benchmarkHaskellDepends = with self; [ base criterion deepseq text weigh ];
            license = "BSD2";
          };
        my-pkg = let
          buildDepends = with self; [
            protolude
            megaparsec
            tasty-th
            HUnit
            tasty-hunit
            tasty-quickcheck
            ansi-wl-pprint
            neat-interpolation
            either
            quickcheck-instances
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
              hpack
              hscolour
              (hoogleLocal {
                packages = buildDepends;
              })
            ];
          };
      };
}).my-pkg.env
