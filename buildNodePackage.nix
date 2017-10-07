{ stdenv, linkNodeDeps }:
{ name # String
, version # String
, src # Drv
, nodeBuildInputs # Listof { name : String, drv : Drv }
, ... }@args:

# since we skip the build phase, pre and post will not work
# the caller gives them with no buildPhase
assert (args ? preBuild || args ? postBuild) -> args ? buildPhase;
# same for configurePhase
assert (args ? preConfigure || args ? postConfigure) -> args ? configurePhase;

with stdenv.lib;

stdenv.mkDerivation ({
  name = "${name}-${version}";
  inherit version src;

  configurePhase = args.configurePhase or "true";
  # skip the build phase except when given as attribute
  dontBuild = !(args ? buildPhase);

  # TODO: maybe we can enable tests?
  doCheck = false;

  installPhase = ''
    runHook preInstall
    mkdir $out

    # a npm package is just the tarball extracted to $out
    cp -r . $out

    echo "HELLO IM HERE"
    # then a node_modules folder is created for all its dependencies
    ${if nodeBuildInputs != []
      then ''
        ln -sT "${linkNodeDeps name nodeBuildInputs}" $out/node_modules
      '' else ''
        echo NO DEPENDENCIES AT ALL!!!
      ''}

    runHook postInstall
  '';

  dontStrip = true; # stolen from npm2nix

}
  // (removeAttrs args [ "nodeBuildInputs" ]))
