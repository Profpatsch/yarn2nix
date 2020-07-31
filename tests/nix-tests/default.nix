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

  # convert a package.json to yarn2nix package template
  template = package-json: pkgs.runCommandLocal "generate-template" {} ''
    ${yarn2nix}/bin/yarn2nix --template ${package-json} > $out
    echo "template for ${package-json} is:" >&2
    cat $out >&2
  '';

  # test suite
  tests = runTestsuite "yarn2nix" [
    (it "checks the template output"
      (let tmpl = import (template my-package-json) {} {};
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
