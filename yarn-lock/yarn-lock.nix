{ mkDerivation, ansi-wl-pprint, base, containers, either, hpack
, lib, megaparsec, neat-interpolation, protolude
, quickcheck-instances, tasty, tasty-hunit, tasty-quickcheck
, tasty-th, text
}:
mkDerivation {
  pname = "yarn-lock";
  version = "0.6.2";
  src = ./.;
  libraryHaskellDepends = [
    base containers either megaparsec protolude text
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    ansi-wl-pprint base containers either megaparsec neat-interpolation
    protolude quickcheck-instances tasty tasty-hunit tasty-quickcheck
    tasty-th text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/Profpatsch/yarn-lock#readme";
  description = "Represent and parse yarn.lock files";
  license = lib.licenses.mit;
}
