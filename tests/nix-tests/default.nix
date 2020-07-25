{ nixpkgsPath ? ../../nixpkgs-pinned.nix
, nixLibPath ? ../../nix-lib
, yarn2nix ? import ../../. { inherit nixpkgsPath; }
}:
let
  pkgs = import nixpkgsPath {};
  nixLib = pkgs.callPackage nixLibPath {
    inherit yarn2nix;
  };

  inherit (import vendor/runTestsuite.nix { inherit pkgs; })
    runTestsuite
    it
    assertEq
    ;

  package-json-with-license = license: pkgs.writeText "package.json" (builtins.toJSON {
    name = "my-package";
    version = "1.5.3";
    license = "${license}";
  });

  # small test package.json
  my-package-json = package-json-with-license "ISC";

  # convert a package.json to yarn2nix package template
  template = package-json: pkgs.runCommandLocal "generate-template" {} ''
    ${yarn2nix}/bin/yarn2nix --template ${package-json} > $out
    echo "template for ${package-json} is:" >&2
    cat $out >&2
  '';

  # test suite
  tests = runTestsuite "yarn2nix" [
    (it "checks the template output"
      (let tmpl = template my-package-json;
      in [
      # TODO: this is a naïve match, might want to create a better test
      (assertEq "template" (import tmpl {} {}) {
        key = {
          name = "my-package";
          scope = "";
        };
        version = "1.5.3";
        nodeBuildInputs = [];
        meta = {
          description = "";
          homepage = "";
          license = "ISC";
        };
      })
      (assertEq "called template" (nixLib.callTemplate tmpl {}) {
        key = {
          name = "my-package";
          scope = "";
        };
        version = "1.5.3";
        nodeBuildInputs = [];
        meta = {
          description = "";
          homepage = "";
          license = pkgs.lib.licenses.isc;
        };
      })
    ]))
    (it "checks license conversion"
      (builtins.map (x: assertEq x.i (nixLib.callTemplate
        (template (package-json-with-license x.i)) {}).meta.license x.o)
      [
        { i = "UNLICENSED"; o = pkgs.lib.licenses.unfree; }
        { i = "SEE LICENSE for info"; o = "SEE LICENSE for info"; }
        # TODO this attr name is depreacated on nixpkgs master, replace with agpl3Only
        { i = "AGPL-3.0-only"; o = pkgs.lib.licenses.agpl3; }
        { i = "GPL-3.0-or-later"; o = pkgs.lib.licenses.gpl3Plus; }
        { i = "BSD-3-Clause"; o = pkgs.lib.licenses.bsd3; }
        { i = "MIT"; o = pkgs.lib.licenses.mit; }
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
