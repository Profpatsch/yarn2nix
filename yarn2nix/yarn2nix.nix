{ mkDerivation, aeson, aeson-better-errors, async-pool, base
, bytestring, containers, data-fix, directory, filepath, hnix
, hpack, lib, mtl, neat-interpolation, optparse-applicative
, prettyprinter, process, protolude, regex-tdfa, scientific, stm
, tasty, tasty-hunit, tasty-quickcheck, tasty-th, text
, transformers, unix, unordered-containers, yarn-lock
}:
mkDerivation {
  pname = "yarn2nix";
  version = "0.10.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors async-pool base bytestring containers
    data-fix directory filepath hnix mtl optparse-applicative
    prettyprinter process protolude regex-tdfa scientific stm text
    transformers unordered-containers yarn-lock
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-better-errors async-pool base bytestring containers
    data-fix directory filepath hnix mtl optparse-applicative
    prettyprinter process protolude regex-tdfa scientific stm text
    transformers unix unordered-containers yarn-lock
  ];
  testHaskellDepends = [
    aeson aeson-better-errors async-pool base bytestring containers
    data-fix directory filepath hnix mtl neat-interpolation
    optparse-applicative prettyprinter process protolude regex-tdfa
    scientific stm tasty tasty-hunit tasty-quickcheck tasty-th text
    transformers unordered-containers yarn-lock
  ];
  prePatch = "hpack";
  homepage = "https://github.com/Profpatsch/yarn2nix#readme";
  description = "Convert yarn.lock files to nix expressions";
  license = lib.licenses.mit;
}
