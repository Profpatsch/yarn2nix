{ pkgs ? import ../../nixpkgs-pinned.nix {}
, nixLibPath ? ../../nix-lib
, yarn2nix ? import ../../. { inherit pkgs; }
}:
let
  nixLib = pkgs.callPackage nixLibPath {
    inherit yarn2nix;
  };

  inherit (import vendor/runTestsuite.nix { inherit pkgs; })
    runTestsuite
    it
    assertEq
    ;

  # small test package.json
  my-package-json = pkgs.writeText "package.json" (builtins.toJSON {
    name = "my-package";
    version = "1.5.3";
  });

  # very simple package depending on moment.js
  readme-example = rec {
    momentjsVersion = "^2.27.0";
    yarn-lock = pkgs.writeText "yarn.lock" ''
      moment@^2.27.0:
        version "2.27.0"
        resolved "https://registry.yarnpkg.com/moment/-/moment-2.27.0.tgz#8bff4e3e26a236220dfe3e36de756b6ebaa0105d"
        integrity sha512-al0MUK7cpIcglMv3YF13qSgdAIqxHTO7brRtaz3DlSULbqfazqkc5kEjNrLDOM7fsjshoFIihnU8snrP7zUvhQ==
    '';
    package-json = pkgs.writeText "package.json" (builtins.toJSON {
      name = "readme-example";
      version = "0.1.0";
      dependencies = {
        moment = momentjsVersion;
      };
    });
    src = pkgs.runCommandLocal "readme-example" {} ''
      mkdir -p $out
      cp ${yarn-lock} $out/yarn.lock
      cp ${package-json} $out/package.json
    '';
  };

  # test suite
  tests = runTestsuite "yarn2nix" [
    (it "checks the template output"
      (let tmpl = nixLib.callPackageJson my-package-json {} {};
      in [
      # TODO: this is a naïve match, might want to create a better test
      (assertEq "template" tmpl {
        key = {
          name = "my-package";
          scope = "";
        };
        version = "1.5.3";
        nodeBuildInputs = [];
        meta = {
          description = "";
          homepage = "";
          license = "";
        };
      })
    ]))
    (it "checks the readme example"
      (let
        tmpl = nixLib.callPackageJson readme-example.package-json {};
        deps = nixLib.callYarnLock readme-example.yarn-lock {};
        pkg  = nixLib.buildNodePackage ({ inherit (readme-example) src; } //
          tmpl (nixLib.buildNodeDeps deps));
      in [
      # TODO we can probably check more here, but this seems like a
      # good sanity check, since its mostly about building successfully
      (assertEq "momentjs linked" (builtins.readDir "${pkg}/node_modules") {
        ".bin" = "directory";
        "moment" = "symlink";
      })]))
  ];

  # small helper that checks the output of tests
  # and pretty-prints errors if there were any
  runTests = pkgs.runCommandLocal "run-tests" {
    testOutput = builtins.toJSON tests;
    passAsFile = [ "testOutput" ];
  }
    (if tests == {}
     then ''touch $out''
     else ''
       echo "ERROR: some tests failed:" >&2
       cat "$testOutputPath" | ${pkgs.jq}/bin/jq >&2
       exit 1
     '');

in {
  inherit runTests;
  testOverriding = import ./test-overriding.nix {
    inherit pkgs nixLib yarn2nix;
  };
}
