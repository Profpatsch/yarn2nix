{ stdenv, linkNodeDeps, nodejs, yarn2nix
, callTemplate, buildTemplate, buildNodeDeps, buildCallDeps }:
{ key # { scope: String, name: String }
, version # String
, src # Drv
, nodeBuildInputs ? null # Listof { key: { scope: String, name: String }, drv : Drv }
, yarnLock ? null
, packageJson ? null
, ... }@args:

# since we skip the build phase, pre and post will not work
# the caller gives them with no buildPhase
assert (args ? preBuild || args ? postBuild) -> args ? buildPhase;
# same for configurePhase
assert (args ? preConfigure || args ? postConfigure) -> args ? configurePhase;

assert nodeBuildInputs == null -> (yarnLock != null && packageJson != null);

with stdenv.lib;

let
  # TODO: scope should be more structured somehow. :(
  packageName =
    if key.scope == ""
    then "${key.name}-${version}"
    else "${key.scope}-${key.name}-${version}";
  # TODO take name and version from here if missing
  autoTemplate =
    callTemplate
      (buildTemplate { inherit packageJson; })
      (buildNodeDeps (buildCallDeps { inherit yarnLock; }));
  nodeDeps =
    if nodeBuildInputs != null
    then nodeBuildInputs
    else autoTemplate.nodeBuildInputs;

in stdenv.mkDerivation ((removeAttrs args [ "key" "nodeBuildInputs" ]) // {
  name = packageName;
  inherit version src;

  buildInputs = [ nodejs ];

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

    # the binaries should be executable (TODO: always on?)
    ${yarn2nix}/bin/node-package-tool \
      set-bin-exec-flag \
      --package $out

    # then a node_modules folder is created for all its dependencies
    ${if nodeDeps != []
      then ''
        rm -rf $out/node_modules
        ln -sT "${linkNodeDeps {
            name = packageName;
            dependencies = nodeDeps;
          }}" $out/node_modules
      '' else ""}

    runHook postInstall
  '';

  dontStrip = true; # stolen from npm2nix

})
