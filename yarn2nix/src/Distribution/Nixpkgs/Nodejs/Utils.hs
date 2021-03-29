{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, LambdaCase #-}
{-|
Description: Misc utils
-}
module Distribution.Nixpkgs.Nodejs.Utils where
import Protolude
import Nix.Expr
import qualified Yarn.Lock.Types as YLT

-- | Representation of a PackageKey as nix attribute name.
packageKeyToSymbol :: YLT.PackageKey -> Text
packageKeyToSymbol (YLT.PackageKey{..}) =
  packageKeyNameToSymbol name <> "@" <> npmVersionSpec
{-# INLINABLE packageKeyToSymbol #-}

-- | Representation of a PackageKeyName as nix attribute name.
packageKeyNameToSymbol :: YLT.PackageKeyName -> Text
packageKeyNameToSymbol = \case
  YLT.SimplePackageKey n -> n
  YLT.ScopedPackageKey scope n -> "@" <> scope <> "/" <> n
{-# INLINABLE packageKeyNameToSymbol #-}

-- | Return a 'Binding' if 'Just' (or none if 'Nothing')
--   for 'mkRecSet' and 'mkNonRecSet'.
attrSetMay :: Text -> Maybe NExpr -> [Binding NExpr]
attrSetMay k v = maybeToList $ (k $=) <$> v
{-# INLINABLE attrSetMay #-}

-- | Convenience shortcut for @'attrSetMay' x (mkStr \<$\> y)@.
attrSetMayStr :: Text -> Maybe Text -> [Binding NExpr]
attrSetMayStr k = attrSetMay k . fmap mkStr
{-# INLINABLE attrSetMayStr #-}
