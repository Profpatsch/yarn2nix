# yarn2nix

```
yarn2nix [--offline] [path/to/yarn.lock]

  Convert a `yarn.lock` into a synonymous nix expression.
  If no path is given, search for `./yarn.lock`.
  If --offline is given, abort if figuring out a hash
  requires network access.

yarn2nix --template [path/to/package.json]

  Generate a package template nix-expression for your `package.json`.
```

## Features

- Purely transform `yarn.lock` files into very minimal, line-diffable nix expressions.
- Nix is used to its fullest. Every package is a derivation, whole dependency
  subtrees are shared in an optimal way, even between projects.
- The ability to resolve git dependencies by prefetching their repos and including the hashes.
- Completely local transformation if there are no git dependencies (can be used inside nix-build, no large file check-in).
- Extremely fast.
- Nice code that can be easily extended, new repositories introduced, adapt to new versions of the `yarn.lock` format.
- Comes with a [nix library][nix-lib] that uses the power of overlays to make overriding dependencies possible.
- POWERED BY [HNIX](https://github.com/haskell-nix/hnix)™ since before it was cool.

Probably a few more.

## Example Output

The [CodiMD server](https://github.com/codimd/server) is an elaborate npm package with hundreds of
dependencies. `yarn2nix` flawlessly parses the current (2020-07) `yarn.lock`
file distributed with the project, including resolving their manual git forks of
multiple npm packages:

```
$ yarn2nix ~/tmp/server/yarn.lock | wc
   7320   22701  399111
$ wc ~/tmp/server/yarn.lock
 11938  18615 500078 /home/lukas/tmp/server/yarn.lock
```

The output of this conversion [can be seen
here](https://gist.github.com/sternenseemann/0c253305350b2406e38c700b840869f2). Also
note that [git dependencies are resolved
correctly](https://gist.github.com/sternenseemann/0c253305350b2406e38c700b840869f2#file-codimd-dependencies-nix-L2086-L2087).

Pushing it through the provided [library of nix
functions][nix-lib], we get a complete build of CodiMD's
dependencies, using the project template (generated with `--template`), we also
build the CodiMD server. Included executables will be in `node_modules/.bin` as expected and
correctly link to their respective library paths in the nix store, for example:

```
$ /nix/store/zs9jk7yhdxsasn26m0903fq89cmyllzv-CodiMD-1.6.0/node_modules/.bin/markdown-it -v
10.0.0
$ readlink /nix/store/zs9jk7yhdxsasn26m0903fq89cmyllzv-CodiMD-1.6.0/node_modules/.bin/markdown-it
/nix/store/bgas2l5izznq1b61a3jyf3gpb73x8chn-markdown-it-10.0.0/bin/markdown-it.js
```

[nix-lib]: ./nix-lib/default.nix

## Building `yarn2nix`

```
$ nix-build
$ result/bin/yarn2nix
```

## Using the generated nix files to build a project

**Note:** This is a temporary interface. Ideally, the library will be in nixpkgs
and yarn2nix will be callable from inside the build (so the resulting nix files
don’t have to be checked in).

Once you have the `yarn2nix` binary, use it to generate nix files for the
`yarn.lock` file and the `package.json`:

```shell
$ yarn2nix ./jsprotect/yarn.lock > npm-deps.nix
$ yarn2nix --template ./jsproject/package.json > npm-package.nix
```

Then use the library to assemble the generated files in a `default.nix`:

```nix
let
  pkgs = import <nixpkgs> {};
  yarn2nix = import /path/to/yarn2nix {};
  nixLib = yarn2nix.nixLib;

in
  nixLib.buildNodePackage
    ( { src = nixLib.removePrefixes [ "node_modules" ] ./.; } //
      nixLib.callTemplate ./npm-package.nix
        (nixLib.buildNodeDeps (pkgs.callPackage ./npm-deps.nix {})))
```

Finally, run `nix-build`, and voilà, in `./result/` you find the project with
all its dependencies correctly linked to their corresponding `node_modules`
folder, recursively.

## Using private package repository

Since `yarn2nix` uses standard `fetchurl` to download packages,
it is possible to authenticate by overriding `fetchurl`
to use the access credentials in `/etc/nix/netrc`.

Refer to the [Enterprise NixOS Wiki article](https://nixos.wiki/wiki/Enterprise)
for instructions.

## Development

```
$ nix-shell
nix-shell> hpack
nix-shell> cabal build yarn2nix
```
