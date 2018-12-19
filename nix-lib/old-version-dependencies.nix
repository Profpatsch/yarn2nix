{ lib, pkgs }:
self: super: {
  yarn-lock =
    super.mkDerivation rec {
      pname = "yarn-lock";
      version = "0.6.0";
      # src = ../../../haskell/yarn-lock;
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "yarn-lock";
        rev = version;
        sha256 = "00i3vq6gn6pkj46nbiywc1a6c5m96bs03mzqw5mb42236fbgsp62";
      };
      license = lib.licenses.mit;
      buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck quickcheck-instances ];
      buildTools = [ self.hpack ];
      prePatch = ''hpack'';
    };
}
