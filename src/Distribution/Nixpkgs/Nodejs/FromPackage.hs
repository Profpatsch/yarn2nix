{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Distribution.Nixpkgs.Nodejs.FromPackage where

import Protolude
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import GHC.Generics (Generic)

import qualified Data.Aeson as A
import Nix.Expr
import Nix.Expr.Additions

import Yarn.Lock (PackageKey(..))
import Distribution.Nixpkgs.Nodejs.Utils (packageKeyToIdentifier)

data Package = Package
  { name :: Text
  , version :: Text
  , description :: Text
  , private :: Bool
  , scripts :: Map Text Text
  , license :: Text
  , author :: Text
  , dependencies :: Dependencies
  , devDependencies :: Dependencies
  } deriving (Show, Eq, Generic)

type Dependencies = Map Text Text
depsToPkgKeys :: Dependencies -> [PackageKey]
depsToPkgKeys = map (\(k, v) -> PackageKey k v) . M.toList

instance A.FromJSON Package

-- | convenience
decode :: BL.ByteString -> Either Text Package
decode = first toS . A.eitherDecode

-- | generate a nix expression that translates your package.nix
--
-- and can serve as template for manual adjustments
genTemplate :: Package -> NExpr
genTemplate Package{..} =
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
            [ "description" $= mkStr description
            , "license"     $= mkStr license ])
        ])
  where
    depPkgKeys = depsToPkgKeys dependencies
    pkgDep depsSym pk = mkSym depsSym !!. packageKeyToIdentifier pk
    nodeDepsSym = "allDeps"
    nameStr = mkStrQ [StrQ name, "-", AntiQ "version"]
