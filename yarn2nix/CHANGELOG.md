# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

All changes are documented in more detail in their respective commit messages.

## [0.9.0] - 2021-08-24

### Added

- `--offline` flag to abort if network would be required 

  If `--offline` is given, yarn2nix will abort with an error message if
  `nix-prefetch-git` would be necessary to use
  (not all yarn.lock files can be converted to nix without network access)
  
- `--template`: add proper SPDX license to nix derivation

  Converts the SPDX license to a nix library license,
  so that the template has the correct license.
  This could be extended to all node_module dependencies in the future.
  
### Fixed

- Fix binary name for scoped packages

  Fix short form of bin field (`"bin": "./path/to/bin"`) for scoped package
  where the binary name should be the package name without scope
  instead of the full name.

  For example for @babel/parser it should generate a `.bin/parser` link
  instead of a `.bin/@babel/parser` link.

- Fix `--template` generation

  Since d607336 buildNodePackage doesn't
  accept a name argument anymore, but a key one. Due to an oversight in
  that change yarn2nix --template would still generate name attributes
  which causes buildNodePackage to fail if directly used with callTemplate
  and an automatically generated template.

- `node-package-tool`: default to no (dev) dependencies if field unparsable

  In the node world you can't depend on anything, especially not that a
  certain field of package.json is well-formed.

  The dependencies field is sometimes malformed, probably in most cases it
  is safe to default to {}. I have observed this in the wild with JSV 0.4.2
  (https://github.com/garycourt/JSV) where dependencies is [] instead of
  {} or being missing. I think packages with malformed dependency field
  can't reasonably expect their dependencies to be installed correctly.

  We only default to {} in cases where we can expect beyond a reasonable
  doubt that there are no dependencies being expressed by the field:

  - Empty array
  - Scalars (Number, String, …)

  We fail parsing if the field is malformed and seems to contain
  something:

  - Non-empty array
  - Object that can't be parsed to Dependencies (i. e. is malformed)

  We use this strategy for both dependencies and devDependencies.
  
- `node-package-tool`: create target directory if it doesn’t exist

  Apparently some namespaced packages have binaries which live in a
  subdirectory. This would previously crash the build.

## [0.8.0] - 2019-12-22

### Added

- Preliminary support for local packages

  To support local packages (see commit for 0.6.0 in yarn-lock), we have to add a new package function to the nix output.

  For now it will only add the path directly, which has the drawback that the nix expression is now required to be in the right position (for relative paths). It should be changed to be user-defined later (i.e. the user passes a function which takes the string of the local path and returns a derivation to the package tarball).

  We don’t use the tarball hash from the lockfile yet to verify the integrity when importing into the nix store.

### Changed

- Bumped `hnix` to `0.6.*`
  - Small breaking change, because the pretty print library changed to `prettyprint`.
- Bumped `yarn-lock` to `0.6.2`

### Fixed

- nix-lib: Allow creating a scoped directory if it exists

## [0.7.0] - 2018-06-14

### Added

- Support for [scoped npm](https://docs.npmjs.com/misc/scope) packages
  - These are used mostly for Typescript annotations, but have been cropping up in other cases as well
  - The support is first-class and scoped packages are already recognized by the `yarn.lock` parser; as such every corner case should be supported (if not it’s a bug or missed type signature change)

### Changed

- `nix-lib`
  - `buildNodeDeps` now takes an overlay instead of a path; for the generated files this means `pkgs.callPackage ./npm-deps.nix {}` instead of just `./npm-deps.nix`
  - Because of scoped package support, the package names are now mostly an attributeset of `{ scope: String, name: String }` where an empty string for `scope` means (360) no scope
  - One exception is the `key` argument of `_buildNodePackage`, which accepts a string as well if the package is not scoped, to save on bytes in the generated nix deps file


## [0.6.0] - 2018-05-31

First bigger packaging project: the `pulp` package manager. It is accessible in the new [`yarn2nix-packages`](https://github.com/Profpatsch/yarn2nix-packages) repository.

### Added

- `npmjs.org` registry shortener for nix output
- Extensive usage documentation for `nix-lib`
- Give every binary in `node_modules/.bin` the executable flag
- Pin `nixpkgs` to `unstable` (makes deterministic `nix-build` possible)

### Changed

- `nix-lib` usage has changed considerably; see `./README.md`
- The templates now include `devDependencies`
- Bump `yarn-lock` to `0.4.1`
- Bump `tasty` to `1.1` (nothing changed)
- Bump `hnix` to `0.5` (some signatures/functions changed)

### Fixed

- Correctly generate npm registry paths containing `/` in package names
- Ignore multiple `package.json` fields if they are mistyped/wrong
  - There seem to be no checks on the registry side
  - We print warnings instead (which are checked in the test suite)
- Correctly substitute `nix-prefetch-git` to use the store path
- Don’t substitute the `node_modules` link farm derivation
- Errors are printed to `stderr`

### Removed

- Dependency on `either`, `EitherT` was removed and replaced by `ExceptT`  rom `transformers`


## [0.5.0] - 2017-12-16

### Added

- First working release
- `0.5.0` to signify that it’s not alpha, but not ready for upstream either

