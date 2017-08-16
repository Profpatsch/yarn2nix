{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Distribution.Nixpkgs.Nodejs.Utils where

import Protolude
import qualified Yarn.Lock.Types as YLT

-- | Representation of a PackageKey as nix attribute name.
packageKeyToIdentifier :: YLT.PackageKey -> Text
packageKeyToIdentifier (YLT.PackageKey{..}) = name <> "@" <> npmVersionSpec
