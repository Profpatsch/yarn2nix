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

## Building

```
$ nix-build
$ result/bin/yarn2nix
```

## Development

```
$ nix-shell
nix-shell> hpack
nix-shell> cabal build
```
