{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Distribution.Nixpkgs.Nodejs.Utils where

import Protolude
import Yarn.Lock (PackageKey(..))

-- | Representation of a PackageKey as nix attribute name.
packageKeyToIdentifier :: PackageKey -> Text
packageKeyToIdentifier pk = name pk <> "@" <> npmSemver pk
