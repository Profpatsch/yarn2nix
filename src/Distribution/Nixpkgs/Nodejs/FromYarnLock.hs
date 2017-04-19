{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Distribution.Nixpkgs.Nodejs.FromYarnLock
( toStdout
, mkPackageSet
) where

import Protolude
import Data.Fix (Fix(Fix))
import Nix.Expr
import Nix.Pretty (prettyNix)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Map.Strict as M
import Text.Regex.TDFA.Text ()
import Text.Regex.TDFA ((=~))

import Yarn.Lock

-- | Pretty print the nixpkgs version of @yarn.lock@ to stdout.
toStdout  :: Lockfile -> IO ()
toStdout = PP.putDoc . prettyNix . mkPackageSet

-- | The name of the function to build node packages.
buildNodePackageSym :: Text
buildNodePackageSym = "buildNodePackage"

-- | The name of the self attribute.
selfPkgSym :: Text
selfPkgSym = "self"

-- | The name of the fix function.
fixSym :: Text
fixSym = "fix"


-- TODO: use the yarn package list to avoid duplication
-- Or probably better: one reclist for deps, then one for assocs.
-- mkPackageSetNoDup :: PackageList -> NExpr
-- mkPackageSetNoDup pl = foldl'

-- BIG TODO: general minification

-- | Convert a @yarn.lock@ to a nix expression.
mkPackageSet :: Lockfile -> NExpr
mkPackageSet lf = params [buildNodePackageSym, fixSym] ==>
  -- enable self-referencing of packages
  -- with string names with a shallow fix
  -- see note FIX
  letE "pkgs" (Param selfPkgSym  ==> pkgSet)
    (mkSym fixSym @@ "pkgs")
  where
    -- | set of all packages
    pkgSet = mkNonRecSet (M.elems $ M.mapWithKey binding lf)
    -- | the binding of a single package
    binding :: PackageKey -> Package -> Binding NExpr
    binding pkgKey pkg = packageKeyToIdentifier pkgKey
                         $$= mkPackage (name pkgKey) pkg

{- NOTE fix
If attributes in a rec set have string names
it’s impossible to reference them.

rec {
  "foo bar" = 1;
  bar = "foo bar" + 2; # doesn’t work
}

instead, a small fix can be used:
let attrs = self: {
    "foo bar" = 1;
    bar = self."foo bar" + 2;
  };
in fix attrs
-}

-- | A single package expression.
mkPackage :: Text -> Package -> NExpr
mkPackage name' pkg = mkSym buildNodePackageSym @@ mkNonRecSet
  [ "name"    $= mkStr (name' <> "-" <> version pkg)
  , "version" $= mkStr (version pkg)
  , "src"     $= fetchSrc (resolved pkg)
  -- TODO: How to handle optional dependencies?
  , "buildInputs"
      $= mkList (((mkSym selfPkgSym) !!.) . packageKeyToIdentifier <$>
        (dependencies pkg <> optionalDependencies pkg))
  ]
  where
    fetchSrc :: RemoteFile -> NExpr
    fetchSrc rf = "fetchgit" @@ mkNonRecSet
                   [ "url"  $= mkStr (url rf)
                   , "sha1" $= mkStr (sha1sum rf) ]

-- | Representation of a PackageKey as nix attribute name.
packageKeyToIdentifier :: PackageKey -> Text
packageKeyToIdentifier pk = name pk <> "@" <> npmSemver pk


-- hnix helpers

-- | Make a binding, but have the key be a string, not symbol.
stringKey :: Text -> NExpr -> Binding NExpr
stringKey k v = NamedVar [dynamicKey k] v
-- | Infix version of 'stringKey'.
($$=) :: Text -> NExpr -> Binding NExpr
($$=) = stringKey
infixr 2 $$=

-- | Make a dynamic key name that is only enclosed in double quotes (no antiquotes).
dynamicKey :: Text -> NKeyName NExpr
dynamicKey k = DynamicKey $ Plain $ DoubleQuoted [Plain k]

-- | shortcut to create a list of closed params, like { foo, bar, baz}:
params :: [Text] -> Params NExpr
params = mkParamset . fmap (, Nothing)

(!!.) :: NExpr -> Text -> NExpr
aset !!. k = Fix
  $ NSelect aset
      [(if isPlainSymbol k then StaticKey else dynamicKey) k] Nothing
  where
    -- the nix lexer regex for IDs (symbols) is 
    -- [a-zA-Z\_][a-zA-Z0-9\_\'\-]*
    isPlainSymbol :: Text -> Bool
    isPlainSymbol s = s =~ ("^[a-zA-Z_][a-zA-Z0-9_'-]*$" :: Text)
infixl 8 !!.
