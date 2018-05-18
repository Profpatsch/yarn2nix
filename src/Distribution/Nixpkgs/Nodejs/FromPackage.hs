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

import Distribution.Nixpkgs.Nodejs.Utils (packageKeyToSymbol)
import qualified Distribution.Nodejs.Package as NP
import qualified Yarn.Lock.Types as YLT


depsToPkgKeys :: NP.Dependencies -> [YLT.PackageKey]
depsToPkgKeys = map (\(k, v) -> YLT.PackageKey k v) . HML.toList

-- | generate a nix expression that translates your package.nix
--
-- and can serve as template for manual adjustments
genTemplate :: NP.Package -> NExpr
genTemplate NP.Package{..} =
  simpleParamSet ["stdenv", "buildNodePackage", "removePrefixes"]
  ==> Param nodeDepsSym
  ==> ("buildNodePackage" @@ mkRecSet
        [ "name" $= nameStr
        , "version" $= mkStr version
        , "src" $= ("removePrefixes"
                     @@ mkList [ mkStr "node_modules" ] @@ "./.")
        , "nodeBuildInputs"  $= (letE "a" (mkSym nodeDepsSym)
                                  $ mkList (map (pkgDep "a") depPkgKeys))
        , "meta"      $= (mkNonRecSet
           $ may "description" description
          <> may "license" license
          <> may "homepage" homepage)
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
    nameStr = mkStrQ [StrQ name]
    may k v = [k $= mkStr (fromMaybe mempty v)]
