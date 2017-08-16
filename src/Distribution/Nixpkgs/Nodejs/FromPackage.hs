{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Distribution.Nixpkgs.Nodejs.FromPackage where

import Protolude
import qualified Data.HashMap.Lazy as HML

import Nix.Expr
import Nix.Expr.Additions

import Distribution.Nixpkgs.Nodejs.Utils (packageKeyToIdentifier)
import qualified Distribution.Nodejs.Package as NP
import qualified Yarn.Lock.Types as YLT

depsToPkgKeys :: NP.Dependencies -> [YLT.PackageKey]
depsToPkgKeys = map (\(k, v) -> YLT.PackageKey k v) . HML.toList

-- | generate a nix expression that translates your package.nix
--
-- and can serve as template for manual adjustments
genTemplate :: NP.Package -> NExpr
genTemplate NP.Package{..} =
  simpleParamSet ["stdenv", "buildNodePackage", "filterSourcePrefixes"]
  ==> Param nodeDepsSym
  -- TODO: devDeps
  ==> ("buildNodePackage" @@ mkRecSet
        [ "name" $= nameStr
        , "version" $= mkStr version
        , "src" $= ("filterSourcePrefixes"
                     @@ mkList [ mkStr "node_modules" ] @@ "./.")
        , "nodeBuildInputs"  $= (letE "a" (mkSym nodeDepsSym)
                                  $ mkList (map (pkgDep "a") depPkgKeys))
        , "meta"      $= (mkNonRecSet
           $ may "description" description
          <> may "license" license
          <> may "homepage" homepage)
        ])
  where
    depPkgKeys = depsToPkgKeys dependencies
    pkgDep depsSym pk = mkSym depsSym !!. packageKeyToIdentifier pk
    nodeDepsSym = "allDeps"
    nameStr = mkStrQ [StrQ name, "-", AntiQ "version"]
    may k v = [k $= mkStr (fromMaybe mempty v)]
