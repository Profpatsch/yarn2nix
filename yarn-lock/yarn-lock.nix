{ mkDerivation, ansi-wl-pprint, base, containers, either, hpack
, lib, megaparsec, neat-interpolation, quickcheck-instances, tasty
, tasty-hunit, tasty-quickcheck, tasty-th, text
}:
mkDerivation {
  pname = "yarn-lock";
  version = "0.6.5";
  src = ./.;
  libraryHaskellDepends = [ base containers either megaparsec text ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    ansi-wl-pprint base containers either megaparsec neat-interpolation
    quickcheck-instances tasty tasty-hunit tasty-quickcheck tasty-th
    text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/Profpatsch/yarn2nix#readme";
  description = "Represent and parse yarn.lock files";
  license = lib.licenses.mit;
}
