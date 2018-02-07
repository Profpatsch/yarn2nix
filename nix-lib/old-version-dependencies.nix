{ lib, pkgs }:
self: super: {
  yarn-lock =
    super.mkDerivation rec {
      pname = "yarn-lock";
      version = "0.4.0";
      # src = ../../../haskell/yarn-lock;
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "yarn-lock";
        # rev = version;
        rev = "8a040e8250a8b871e0dafaeffccaf58ff34db0c3";
        sha256 = "1r0k0p6ssay9nsh78f1pwkirp1y2qjwab73a7x2j7b9dp1djf2qx";
      };
      license = lib.licenses.mit;
      buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck quickcheck-instances ];
      buildTools = [ self.hpack ];
      prePatch = ''hpack'';
    };
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
       license = lib.licenses.bsd2;
     };
}
