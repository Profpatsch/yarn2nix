{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
{-|
Description: Generate nix expression for 'NP.Package'
-}
module Distribution.Nixpkgs.Nodejs.FromPackage
( genTemplate
) where

import Protolude
import qualified Data.HashMap.Lazy as HML

import Nix.Expr
import Nix.Expr.Additions

import Distribution.Nixpkgs.Nodejs.Utils (packageKeyToSymbol, attrSetMayStr, attrSetMay)
import qualified Distribution.Nodejs.Package as NP
import qualified Distribution.Nixpkgs.Nodejs.License as NL
import qualified Yarn.Lock.Types as YLT


depsToPkgKeys :: NP.Dependencies -> [YLT.PackageKey]
depsToPkgKeys = map toPkgKey . HML.toList
  where
    toPkgKey (k, v) =
      YLT.PackageKey (parsePackageKeyName k) v

parsePackageKeyName :: Text -> YLT.PackageKeyName
parsePackageKeyName k =
  -- we don’t crash on a “wrong” package key to keep this
  -- code pure, but assume it’s a simple key instead.
  maybe (YLT.SimplePackageKey k) identity
    $ YLT.parsePackageKeyName k

-- | generate a nix expression that translates your package.nix
--
-- and can serve as template for manual adjustments
genTemplate :: Maybe NL.LicensesBySpdxId -> NP.Package -> NExpr
genTemplate licSet NP.Package{..} =
  -- reserved for possible future arguments (to prevent breakage)
  simpleParamSet []
  ==> Param nodeDepsSym
  ==> (mkNonRecSet
        [ "key" $= packageKeyToSet (parsePackageKeyName name)
        , "version" $= mkStr version
        , "nodeBuildInputs"  $= (letE "a" (mkSym nodeDepsSym)
                                  $ mkList (map (pkgDep "a") depPkgKeys))
        , "meta"      $= (mkNonRecSet
           $ attrSetMayStr "description" description
          <> attrSetMay    "license" (NL.nodeLicenseToNixpkgs license licSet)
          <> attrSetMayStr "homepage" homepage)
        ])
  where
    -- TODO: The devDependencies are only needed for the build
    -- and probably also only from packages not stemming from
    -- a npm registry (e.g. a git package). It would be cool
    -- if these dependencies were gone in the final output.
    -- See https://github.com/Profpatsch/yarn2nix/issues/5
    depPkgKeys = depsToPkgKeys (dependencies <> devDependencies)
    pkgDep depsSym pk = mkSym depsSym !!. packageKeyToSymbol pk
    nodeDepsSym = "allDeps"
    packageKeyToSet (YLT.SimplePackageKey n) =
      packageKeyToSet $ YLT.ScopedPackageKey "" n
    packageKeyToSet (YLT.ScopedPackageKey s n) = mkNonRecSet $
      [ bindTo "name"  $ mkStrQ [ StrQ n ]
      , bindTo "scope" $ mkStrQ [ StrQ s ]
      ]
