# yarn2nix

```
yarn2nix [path/to/yarn.lock]
yarn2nix --template [path/to/package.json]

Convert a `yarn.lock` into a synonymous nix expression.
If no path is given, search for `./yarn.lock`.
In the second invocation generate a template for your `package.json`.
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

The [HackMD](https://github.com/hackmdio/hackmd) project is an elaborate npm package with hundreds of
dependencies. `yarn2nix` flawlessly parses the current (2017-12) `yarn.lock`
file distributed with the project, including resolving their manual git forks of
multiple npm packages:

```
dist/build/yarn2nix/yarn2nix ~/tmp/hackmd/yarn.lock | wc
   5306   17068  280246
cat ~/tmp/hackmd/yarn.lock | wc
   7667   11307  266652
```

The output of this conversion [can be seen
here](https://gist.github.com/Profpatsch/9e50d25faf5a5c4269566e9b7d89199b). Also
note that [git dependencies are resolved
correctly](https://gist.github.com/Profpatsch/9e50d25faf5a5c4269566e9b7d89199b#file-hackmd-dependencies-nix-L1291).

Pushing it through the provided [library of nix
functions][nix-lib], we get a complete build of HackMD
dependencies, using the project template (generated with `--template`), we also
build HackMD. Included executables will be in `node_modules/.bin` as expected and
correctly link to their respective library paths in the nix store, for example:

```
 ls /nix/store/2jc8b4q9i2cvx7pamv5r8md45prrvx4f-hackmd-0.5.1-0.5.1/node_modules/.bin/markdown-it --help
Usage: ls [OPTION]... [FILE]...
List information about the FILEs (the current directory by default).
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
  nixpkgsPath = <nixpkgs>;
  pkgs = import nixpkgsPath {};
  nixLib = pkgs.callPackage /path/to/yarn2nix/nix-lib {
    # WARNING (TODO): for now you need to use this checked out yarn2nix
    # because the upstream package (in haskellPackages) might have
    # broken dependencies (yarn-lock and yarn2nix are not in stackage)
    yarn2nix = import /path/to/yarn2nix { inherit nixpkgsPath; };
  };

in
  nixLib.buildNodePackage
    ( { src = nixLib.removePrefixes [ "node_modules" ] ./.; } //
      nixLib.callTemplate ./npm-package.nix
        (nixLib.buildNodeDeps ./npm-deps.nix) )
```

Finally, run `nix-build`, and voilà, in `./result/` you find the project with
all its dependencies correctly linked to their corresponding `node_modules`
folder, recursively.

## Development

```
$ nix-shell
nix-shell> hpack
nix-shell> cabal build
```
