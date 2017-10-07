{ lib, fetchFromGitHub }:
self: super:
super.mkDerivation rec {
  pname = "yarn-lock";
  version = "0.4.0";
  # src = ../../haskell/yarn-lock;
  src = fetchFromGitHub {
    owner = "Profpatsch";
    repo = "yarn-lock";
    rev = version;
    sha256 = "0r5ipyfpdmg9n9zv7q1knrlypv9q2l1x24j1sgibxy411b7r62wv";
  };
  license = lib.licenses.mit;
  buildDepends = with self; [ megaparsec protolude tasty-hunit tasty-th either neat-interpolation tasty-quickcheck ];
  buildTools = [ self.hpack ];
  preConfigure = ''hpack'';
}
