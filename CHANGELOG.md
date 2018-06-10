# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

All changes are documented in more detail in their respective commit messages.

## [0.7.0] - unpublished

### Changed

- `nix-lib`
  - `buildNodeDeps` now takes an overlay instead of a path; for the generated files this means `pkgs.callPackage ./npm-deps.nix {}` instead of just `./npm-deps.nix`


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

