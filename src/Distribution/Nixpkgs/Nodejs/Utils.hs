{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
{-|
Description: Misc utils
-}
module Distribution.Nixpkgs.Nodejs.Utils where
import Protolude
import qualified Yarn.Lock.Types as YLT

-- | Representation of a PackageKey as nix attribute name.
packageKeyToSymbol :: YLT.PackageKey -> Text
packageKeyToSymbol (YLT.PackageKey{..}) = name <> "@" <> npmVersionSpec
