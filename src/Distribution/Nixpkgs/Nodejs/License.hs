{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-|
Description: Convert @package.json@ license fields to nixpkgs license attribute sets
-}
module Distribution.Nixpkgs.Nodejs.License
  ( -- * Conversion Logic
    nodeLicenseToNixpkgs
    -- * License Set Representation
  , NixpkgsLicense (..)
  , unfreeLicense
    -- * License Lookup Table
  , decode
  , LicensesBySpdxId (..)
  , lookupSpdxId
  ) where

import Protolude

import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy as HML
import Nix.Expr
import Distribution.Nixpkgs.Nodejs.Utils (attrSetMay, attrSetMayStr)

-- newtype to circumvent the default instance: we don't want
-- the key of the JSON object to be the key of the HashMap,
-- but one of its values (spdxId).
-- | Lookup table from SPDX identifier (as 'Text') to 'NixpkgsLicense'.
--
--   Should be obtained using 'decode'.
newtype LicensesBySpdxId
  = LicensesBySpdxId { unLicensesBySpdxId :: HML.HashMap Text NixpkgsLicense }
  deriving (Show, Eq, Semigroup, Monoid)

-- | Representation of a nixpkgs license set as found in
--   @lib.licenses@. There doesn't seem to be a strict
--   definition of what is required and what is optional,
--   the distribution of 'Maybe' and non-'Maybe' values
--   is based on the current situation in @lib/licenses.nix@.
data NixpkgsLicense
  = NixpkgsLicense
  { attrName  :: Text       -- ^ Attribute name of the license in @lib.licenses@
  , shortName :: Text
  , spdxId    :: Maybe Text
  , fullName  :: Text
  , url       :: Maybe Text
  , free      :: Maybe Bool
  } deriving (Show, Eq)

-- | Static version of @lib.licenses.unfree@,
--   so @UNLICENSED@ can be handled correctly
--   even if no lookup table is provided.
unfreeLicense :: NixpkgsLicense
unfreeLicense = NixpkgsLicense
  { attrName = "unfree"
  , shortName = "unfree"
  , fullName = "Unfree"
  , free = Just False
  , spdxId = Nothing
  , url = Nothing
  }

instance A.FromJSON LicensesBySpdxId where
  parseJSON = A.withObject "NixpkgsLicenseSet" $
    HMS.foldrWithKey (\k v p -> p >>= addNixpkgsLicense k v) (pure mempty)

addNixpkgsLicense :: Text -> AT.Value -> LicensesBySpdxId -> AT.Parser LicensesBySpdxId
addNixpkgsLicense attr val lics = do
  license <- A.withObject "NixpkgsLicense" parseLicense val
  let (LicensesBySpdxId licsMap) = lics
  -- insert if it has an spdxId, otherwise just return lics
  case spdxId license of
    Nothing -> pure lics
    Just i -> pure $ LicensesBySpdxId $ HML.insert i license licsMap
  where parseLicense v = NixpkgsLicense
          <$> pure attr
          <*> v .:? "shortName" .!= attr
          <*> v .:? "spdxId"
          <*> v .:  "fullName"
          <*> v .:? "url"
          <*> v .:? "free"

-- | Build nix attribute set for given 'NixpkgsLicense'.
--
--   The resulting nix value of @nixpkgsLicenseExpression x@
--   should be equal to @lib.licenses.<attrName x>@ for the
--   same version of nixpkgs used.
nixpkgsLicenseExpression :: NixpkgsLicense -> NExpr
nixpkgsLicenseExpression (NixpkgsLicense{..}) = mkNonRecSet $
  [ "fullName" $= mkStr fullName
  , "shortName" $= mkStr shortName ]
  <> attrSetMayStr "spdxId" spdxId
  <> attrSetMayStr "url" url
  <> attrSetMay "free" (mkBool <$> free)

-- | Implements the logic for converting from an (optional)
--   @package.json@ @license@ field to a nixpkgs @meta.license@
--   set. Since support for multiple licenses is poor in nixpkgs
--   at the moment, we don't attempt to convert SPDX expressions
--   like @(ISC OR GPL-3.0-only)@.
--
--   See <https://docs.npmjs.com/files/package.json#license> for
--   details on npm's @license@ field.
nodeLicenseToNixpkgs :: Maybe Text -> Maybe LicensesBySpdxId -> Maybe NExpr
nodeLicenseToNixpkgs nodeLicense licSet = do
  id <- nodeLicense
  if id == "UNLICENSED"
    then pure $ nixpkgsLicenseExpression unfreeLicense
    else (lookupSpdxId id =<< licSet) <|> pure (mkStr id)

-- | Lookup function for 'LicensesBySpdxId' which directly returns a 'NExpr'.
--   This function only looks up by SPDX identifier and does not take
--   npm-specific quirks into account.
--
--   Use 'nodeLicenseToNixpkgs' when dealing with the @license@ field
--   of a npm-ish javascript package.
lookupSpdxId :: Text -> LicensesBySpdxId -> Maybe NExpr
lookupSpdxId lic licSet =
  nixpkgsLicenseExpression <$> HML.lookup lic (unLicensesBySpdxId licSet)

-- | Build lookup table from a @licenses.json@ file which has been
--   built by @writeText "licenses.json" (builtins.toJSON lib.licenses)@.
--
--   Reexport from aeson.
decode :: BL.ByteString -> Maybe LicensesBySpdxId
decode = A.decode
