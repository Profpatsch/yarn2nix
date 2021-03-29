{ pkgs, nixLib, yarn2nix }:

nixLib.linkNodeDeps {
  name = "test";
  dependencies =
    let allDeps =
      nixLib.buildNodeDeps (pkgs.lib.composeExtensions
        (pkgs.callPackage ./deps.nix {})
        (self: super: {
          # we are able to override an existing package
          # TODO don’t use the { name, drv } version
          # of _buildNodePackage here.
          left-pad = {
            key = { scope = ""; name = "left-pad"; };
            drv = super.left-pad.drv.overrideAttrs (old: {
              buildPhase = ''echo OVERRIDDEN!'';
            });
          };
          # we can also add a new package
          right-pad = super._buildNodePackage {
            key = "right-pad";
            version = "0.0.0.1";
            src = pkgs.runCommand "right-pad-src" {} ''
              mkdir $out
              echo '{ "name": "right-pad", "version": "0.0.0.1" }' \
                > $out/package.json
            '';
            nodeBuildInputs = [ self.left-pad ];
            # which reference the other package
            postInstall = ''
              echo left-pad package.json
              cat $out/node_modules/left-pad/package.json
            '';
          };
        }));
    in [
      allDeps.left-pad
      allDeps.right-pad
    ];
}
