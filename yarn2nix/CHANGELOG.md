# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

All changes are documented in more detail in their respective commit messages.

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

