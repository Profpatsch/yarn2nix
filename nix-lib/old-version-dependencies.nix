{ lib, pkgs }:
self: super: {
  yarn-lock =
    super.mkDerivation rec {
      pname = "yarn-lock";
      version = "0.5.0";
      # src = ../../../haskell/yarn-lock;
      src = pkgs.fetchFromGitHub {
        owner = "Profpatsch";
        repo = "yarn-lock";
        rev = version;
        # rev = "61cc65a858db92e7c0bea861bf279f286c34bb81";
        sha256 = "007a7n1qqa7cp85bbw00yvkg2ikg1yzj1k8i9bav1bnbk6n7xp7s";
      };
      license = lib.licenses.mit;
      buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck quickcheck-instances ];
      buildTools = [ self.hpack ];
      prePatch = ''hpack'';
    };
}
