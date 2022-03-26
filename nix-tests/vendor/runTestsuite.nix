{ pkgs }:

# Copied from the code I wrote for https://code.tvl.fyi/tree/nix/runTestsuite/default.nix?id=c8e888c1d2c6dfe60a835d1810ab57d87d097e93
# with the typechecking parts removed.

# Run a nix testsuite.
#
# The tests are simple assertions on the nix level,
# and can use derivation outputs if IfD is enabled.
#
# You build a testsuite by bundling assertions into
# “it”s and then bundling the “it”s into a testsuite.
#
# Running the testsuite will abort evaluation if
# any assertion fails.
#
# Example:
#
#   runTestsuite "myFancyTestsuite" [
#     (it "does an assertion" [
#       (assertEq "42 is equal to 42" "42" "42")
#       (assertEq "also 23" 23 23)
#     ])
#     (it "frmbls the brlbr" [
#       (assertEq true false)
#     ])
#   ]
#
# will fail the second it group because true is not false.

let
  lib = pkgs.lib;

  # rewrite the builtins.partition result
  # to use `ok` and `err` instead of `right` and `wrong`.
  partitionTests = pred: xs:
    let res = builtins.partition pred xs;
    in {
      ok = res.right;
      err = res.wrong;
    };

  # assert that left and right values are equal
  assertEq =
    (desc: left: right:
      if left == right
      then { yep = { test = desc; }; }
      else { nope = {
        test = desc;
        inherit left right;
      };
    });

  # Annotate a bunch of asserts with a descriptive name
  it = desc: asserts: {
    it-desc = desc;
    inherit asserts;
  };

  # Run a bunch of its and check whether all asserts are yep.
  # If not, abort evaluation with `throw`
  # and print the result of the test suite.
  #
  # Takes a test suite name as first argument.
  runTestsuite =
    (name: itResults:
      let
        goodIt = it: {
          inherit (it) it-desc;
          asserts = partitionTests (ass:
            if ass ? yep then true
            else if ass ? nope then false
            else abort "assertion result should be either yep or nope"
          ) it.asserts;
        };
        goodIts = partitionTests (it: (goodIt it).asserts.err == []);
        res = goodIts itResults;
      in
        if res.err == []
        then {}
        # TODO(Profpatsch): pretty printing of results
        # and probably also somewhat easier to read output
        else res);

in {
  inherit
    assertEq
    it
    runTestsuite
    ;
}
