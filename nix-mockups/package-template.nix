{ stdenv, filterSourcePrefixes, buildNodePackage }:
# dependency set built by yarn2nix, suitable for `linkNodeDeps`
allDeps:

buildNodePackage {
  name = "testpkg-${version}";
  version = "1.2.3";
  src = filterSourcePrefixes ["node_modules"] ./.;

  nodeBuildInputs = let a = allDeps; [a."ember-cli@2.13.1" a."ember-cli-htmlbars@^1.3.0"];

  meta = {
    description = "foo";
    license = "MIT";
  };
};
