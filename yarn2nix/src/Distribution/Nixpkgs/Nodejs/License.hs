{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
  {-|
Description: Convert @package.json@ license fields to nixpkgs license attribute sets
-}
module Distribution.Nixpkgs.Nodejs.License
  ( -- * Conversion Logic
    nodeLicenseToNixpkgs
    -- * License Lookup Table
  , LicensesBySpdxId
  ) where

import Protolude

import qualified Data.Aeson as A
import qualified Nix.Expr as Nix
import qualified Data.Map.Strict as Map
import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Scientific as Scientific

-- newtype to circumvent the default instance: we don't want
-- the key of the JSON object to be the key of the HashMap,
-- but one of its values (spdxId).
-- | Lookup table from SPDX identifier (as 'Text') to 'NixpkgsLicense'.
newtype LicensesBySpdxId
  = LicensesBySpdxId (Map Text NixpkgsLicense)
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Representation of a nixpkgs license set as found in
--   @lib.licenses@. There doesn't seem to be a strict
--   definition of what is required and what is optional,
--   the distribution of 'Maybe' and non-'Maybe' values
--   is based on the current situation in @lib/licenses.nix@.
data NixpkgsLicense
  = NixpkgsLicense ([(Text, LicenseValue)])
  deriving stock (Show, Eq)
data LicenseValue
  = LText Text
  | LBool Bool
  | LInt Int
  deriving stock (Show, Eq)

-- | Static version of @lib.licenses.unfree@,
--   so @UNLICENSED@ can be handled correctly
--   even if no lookup table is provided.
--
-- TODO: this will go out of sync with the nixpkgs definitions every once in a while, how to fix?
unfreeLicense :: NixpkgsLicense
unfreeLicense = NixpkgsLicense $ [
    ("shortName", LText "unfree")
  , ("deprecated", LBool False)
  , ("fullName", LText "Unfree")
  , ("redistributable", LBool False)
  , ("free", LBool False)
 ]

instance A.FromJSON LicensesBySpdxId where
  parseJSON = Json.toAesonParser identity ((Json.forEachInObject $ \_key -> do
    Json.keyMay "spdxId" Json.asText
    >>= \case
      Nothing -> pure Nothing
      Just spdxId -> do
        spdxLicense <- (Json.eachInObject $ Json.withValue $ \case
          A.String t -> Right $ LText t
          A.Bool b -> Right $ LBool b
          A.Number s -> case Scientific.toBoundedInteger @Int s of
            Just i -> Right $ LInt i
            Nothing -> Left $ "Not an integer: " <> (s & show)
          A.Null -> Left "Cannot parse Null as license value for now"
          A.Object _ -> Left "Cannot parse Object as license value for now"
          A.Array _ -> Left "Cannot parse Array as license value for now")
          <&> NixpkgsLicense
        pure $ Just (spdxId, spdxLicense)
   )
      <&> catMaybes
      <&> Map.fromList
      <&> LicensesBySpdxId
   )


-- | Build nix attribute set for given 'NixpkgsLicense'.
--
--   The resulting nix value of @nixpkgsLicenseExpression x@
--   should be equal to @lib.licenses.<attrName x>@ for the
--   same version of nixpkgs used.
nixpkgsLicenseExpression :: NixpkgsLicense -> Nix.NExpr
nixpkgsLicenseExpression (NixpkgsLicense m) =
  m
  <&> second licenseValueToNExpr
  & Nix.attrsE

licenseValueToNExpr :: LicenseValue -> Nix.NExpr
licenseValueToNExpr = \case
  LText t -> Nix.mkStr t
  LInt i -> Nix.mkInt (i & fromIntegral @Int @Integer)
  LBool b -> Nix.mkBool b

-- | Implements the logic for converting from an (optional)
--   @package.json@ @license@ field to a nixpkgs @meta.license@
--   set. Since support for multiple licenses is poor in nixpkgs
--   at the moment, we don't attempt to convert SPDX expressions
--   like @(ISC OR GPL-3.0-only)@.
--
--   See <https://docs.npmjs.com/files/package.json#license> for
--   details on npm's @license@ field.
nodeLicenseToNixpkgs :: Text -> LicensesBySpdxId -> Nix.NExpr
nodeLicenseToNixpkgs nodeLicense licSet = do
  if nodeLicense == "UNLICENSED"
    then nixpkgsLicenseExpression unfreeLicense
    else case lookupSpdxId nodeLicense licSet of
      Nothing -> Nix.mkStr nodeLicense
      Just license -> license

-- | Lookup function for 'LicensesBySpdxId' which directly returns a 'NExpr'.
--   This function only looks up by SPDX identifier and does not take
--   npm-specific quirks into account.
--
--   Use 'nodeLicenseToNixpkgs' when dealing with the @license@ field
--   of a npm-ish javascript package.
lookupSpdxId :: Text -> LicensesBySpdxId -> Maybe Nix.NExpr
lookupSpdxId lic (LicensesBySpdxId licSet) =
  licSet
  & Map.lookup lic
  <&> nixpkgsLicenseExpression
