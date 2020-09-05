{ mkDerivation, aeson, async-pool, base, bytestring, containers
, data-fix, directory, filepath, hnix, mtl
, neat-interpolation, optparse-applicative, prettyprinter, process
, protolude, regex-tdfa, regex-tdfa-text, stdenv, stm, tasty
, tasty-hunit, tasty-quickcheck, tasty-th, text, transformers, unix
, unordered-containers, yarn-lock
}:
mkDerivation {
  pname = "yarn2nix";
  version = "0.8.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async-pool base bytestring containers data-fix directory
    filepath hnix mtl optparse-applicative prettyprinter process
    protolude regex-tdfa regex-tdfa-text stm text transformers
    unordered-containers yarn-lock
  ];
  executableHaskellDepends = [
    aeson async-pool base bytestring containers data-fix directory
    filepath hnix mtl optparse-applicative prettyprinter process
    protolude regex-tdfa regex-tdfa-text stm text transformers unix
    unordered-containers yarn-lock
  ];
  testHaskellDepends = [
    aeson async-pool base bytestring containers data-fix directory
    filepath hnix mtl neat-interpolation optparse-applicative
    prettyprinter process protolude regex-tdfa regex-tdfa-text stm
    tasty tasty-hunit tasty-quickcheck tasty-th text transformers
    unordered-containers yarn-lock
  ];
  homepage = "https://github.com/Profpatsch/yarn2nix#readme";
  description = "Convert yarn.lock files to nix expressions";
  license = stdenv.lib.licenses.mit;
}
