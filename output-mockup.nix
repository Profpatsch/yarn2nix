{ buildNodePackage, fetchurl, fix }:
let
  # type : AttrsOf Prefix [Prefix : (Name -> Version -> URL)]
  prefixes = {
    yarnpkg n v = "https://registry.yarnpkg.com/${n}/-/${n}-${v}.tgz"
  };

  # type : Name -> Version -> Prefix -> Sha1Sum -> Dependencies -> NodePackage
  shortBuildPkg = name: version: prfx: sha1: deps: buildNodePackage {
    inherit name version;
    src = fetchurl {
      url = prfx name version;
      inherit sha1;
    };
    nodeBuildInputs = deps;
  };

  #### shortcuts (bring filesize down)
  y = prefixes.yarnpkg;
  b = shortBuildPkg

  # type : AttrsOf NodePackage -> AttrsOf NodePackage
  # fix’ed deep down below. :)
  pkgs = s: {
    "accepts@~1.3.3" = "accepts@1.3.3";
    "accepts@1.3.3" = b "accepts" "1.3.3" y "sha" [];
    "babel-core@^6.14.0" = s."babel-core@6.24.1";
    "babel-core@6.24.1" = b "babel-core" "6.24.1" y "a0e457c58ebdbae575c9f8cd75127e93756435d8" [
        s."accepts@~1.3.3"
    ];
  };

in fix pkgs;
