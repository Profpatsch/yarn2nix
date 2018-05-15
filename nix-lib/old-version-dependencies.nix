{ lib, pkgs }:
self: super: {
  yarn-lock =
    super.mkDerivation rec {
      pname = "yarn-lock";
      version = "0.4.1";
      # src = ../../../haskell/yarn-lock;
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "yarn-lock";
        rev = version;
        # rev = "61cc65a858db92e7c0bea861bf279f286c34bb81";
        sha256 = "1fs440qf1c2w3dav82f1bsdi9k5cms106bg40ny33n0f0427mdpm";
      };
      license = lib.licenses.mit;
      buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck quickcheck-instances ];
      buildTools = [ self.hpack ];
      prePatch = ''hpack'';
    };
}
