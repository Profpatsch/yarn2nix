{ mkDerivation, aeson, ansi-wl-pprint, async-pool, base, bytestring
, containers, data-fix, directory, either, filepath, hnix
, http-client, http-client-tls, monad-par, mtl, neat-interpolation
, optparse-applicative, process, protolude, regex-tdfa
, regex-tdfa-text, stdenv, stm, tasty, tasty-hunit
, tasty-quickcheck, tasty-th, text, unix, unordered-containers
, yarn-lock
}:
mkDerivation {
  pname = "yarn2nix";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint async-pool base bytestring containers data-fix
    directory either filepath hnix http-client http-client-tls
    monad-par mtl process protolude regex-tdfa regex-tdfa-text stm text
    unordered-containers yarn-lock
  ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint async-pool base bytestring containers data-fix
    directory either filepath hnix http-client http-client-tls
    monad-par mtl optparse-applicative process protolude regex-tdfa
    regex-tdfa-text stm text unix unordered-containers yarn-lock
  ];
  testHaskellDepends = [
    aeson ansi-wl-pprint async-pool base bytestring containers data-fix
    directory either filepath hnix http-client http-client-tls
    monad-par mtl neat-interpolation process protolude regex-tdfa
    regex-tdfa-text stm tasty tasty-hunit tasty-quickcheck tasty-th
    text unordered-containers yarn-lock
  ];
  homepage = "https://github.com/Profpatsch/yarn2nix#readme";
  description = "Convert yarn.lock files to nix expressions";
  license = stdenv.lib.licenses.mit;
}
