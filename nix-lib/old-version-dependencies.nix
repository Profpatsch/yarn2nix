{ lib, pkgs }:
self: super: {
  yarn-lock =
    super.mkDerivation rec {
      pname = "yarn-lock";
      version = "0.6.2";
      # src = ../../../haskell/yarn-lock;
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "yarn-lock";
        rev = version;
        sha256 = "06171ya075yx88gfx39z6mh1k1al0qaqrarbas5mv6lrky19bdxs";
      };
      license = lib.licenses.mit;
      buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck quickcheck-instances ];
      buildTools = [ self.hpack ];
      prePatch = ''hpack'';
    };
}
