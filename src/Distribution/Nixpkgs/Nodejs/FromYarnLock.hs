{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, ViewPatterns, RecordWildCards, NoImplicitPrelude, LambdaCase, NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Distribution.Nixpkgs.Nodejs.FromYarnLock
( toStdout
, mkPackageSet
) where

import Protolude
import Data.Fix (Fix(Fix))
import Nix.Expr
import Nix.Pretty (prettyNix)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.Regex.TDFA.Text ()
import Text.Regex.TDFA ((=~))

import Yarn.Lock (PackageKey(..), Package(..), Lockfile, RemoteFile(..))
import qualified Yarn.Lock as YL


-- | Pretty print the nixpkgs version of @yarn.lock@ to stdout.
toStdout  :: Lockfile -> IO ()
toStdout = PP.putDoc . prettyNix . mkPackageSet

-- CONVENTION: *Sym is a nix symbol
-- We keep them Text, because that’s what they are in hnix
-- TODO: find a way to actually have a nice type for symbols

-- | The name of the function to build node packages.
buildNodePackageSym :: Text
buildNodePackageSym = "buildNodePackage"

-- | The name of the self attribute.
selfPkgSym :: Text
selfPkgSym = "s"

-- | The name of the fix function.
fixSym :: Text
fixSym = "fix"

fetchurlSym :: Text
fetchurlSym = "fetchurl"

-- | Shorthand for antiquoted nix strings
type AntiQ = Antiquoted Text NExpr

-- | Short helper for 'Registry' definition
data RegVal = V AntiQ
            -- ^ nix antiquoted variable
            | T Text
            -- ^ normal nix string

-- | Represents a nodejs registry which resolves node packages
--
-- It will be shortened in the final nix output file.
data Registry = Registry
  { registrySym :: Text
  -- ^ registry symbol name
  , registryPrefix :: Text
  -- ^ prefix string that selects to this prefix
  , buildRegistry :: AntiQ -> AntiQ -> [RegVal]
  -- ^ takes name and version, interpolates them
  }

-- | Shortened alias for a symbol (to bring down filesize)
newtype ShortSym = ShortSym { unShortSym :: Text } deriving (IsString)

yarnRegistry :: Registry
yarnRegistry = Registry "yarnpkg" p
                $ \n v -> [T p, V n, T "/-/", V n, T "-", V v, T ".tgz"]
  where p = "https://registry.yarnpkg.com/"

-- | all Registries with their shortened names
allRegistryTuples :: [(Registry, ShortSym)]
allRegistryTuples = map (fmap ShortSym)
  [ (yarnRegistry, "y")]
-- TODO: include ShortSym in the actual registry?

prefixesSym :: Text
prefixesSym = "prefixes"

-- | combine a binding and its short symbol name
data ShortenedBinding = ShortenedBinding
  { bindingSym :: Text
  , bindingShortSym :: ShortSym
  , binding :: NExpr }

pkgBuildFn :: ShortenedBinding
pkgBuildFn = ShortenedBinding "shortBuildPkg" "b" bnd
  where
    bnd = multiParam ["name", "version", "prfx", "sha1", "deps"] $ mkNonRecSet
           [ inherit [StaticKey "name", StaticKey "version"]
           , "src" $= (mkSym fetchurlSym @@ mkNonRecSet
             [ "url" $= ("prfx" @@ "name" @@ "version")
             , inherit [StaticKey "sha1"] ])
           , "nodeBuildInputs" $= "deps"
           ]

allShortenedBindings :: [ShortenedBinding]
allShortenedBindings = [pkgBuildFn]

registryShortenedBinding :: (Registry, ShortSym) -> ShortenedBinding
registryShortenedBinding (Registry{registrySym, buildRegistry}, s) =
  ShortenedBinding registrySym s $ multiParam ["n", "v"]
    $ regvToNExpr $ buildRegistry (Antiquoted "n") (Antiquoted "v")
  where
    regvToNExpr regvals = Fix . NStr . DoubleQuoted
                          $ flip map regvals $ \case
                              T t -> Plain t
                              V aq -> aq


-- | Static bindings at the top of the output’s let section
staticBindings :: [Binding NExpr]
staticBindings = longDefinitions ++ shortenedDefinitions
  -- TODO: add the comments from output-mockup.nix
  -- see https://github.com/jwiegley/hnix/issues/57
  where
    mkSymBnds, mkShortSymBnds :: [ShortenedBinding] -> [Binding NExpr]
    mkSymBnds = map (\ShortenedBinding{..} -> bindingSym $= binding)
    mkShortSymBnds = map (\ShortenedBinding{..} ->
                            unShortSym bindingShortSym $= binding)
    -- all definitions at the beginning
    longDefinitions = prefixAttrset : mkSymBnds allShortenedBindings
    -- the definition attrset of prefixes
    prefixAttrset = prefixesSym $= mkNonRecSet
      (mkSymBnds $ map registryShortenedBinding allRegistryTuples)
    -- list of shortened definitions
    shortenedDefinitions = mkShortSymBnds $
      (map (ln Nothing)             allShortenedBindings) ++
      (map (ln (Just prefixesSym) . registryShortenedBinding) allRegistryTuples)
    -- link to the full definition that has maybe a prefix
    ln :: Maybe Text -> ShortenedBinding -> ShortenedBinding
    ln expr sb@ShortenedBinding{bindingSym} =
      let replRHS new = sb { binding = new } in
        case expr of
          Nothing  -> replRHS $ mkSym bindingSym
          Just prf -> replRHS $ mkSym prf !!. bindingSym


-- | Convert a @yarn.lock@ to a nix expression.
mkPackageSet :: Lockfile -> NExpr
mkPackageSet (YL.decycle -> lf) =
  simpleParamSet [buildNodePackageSym, fixSym, fetchurlSym] ==>
    -- enable self-referencing of packages
    -- with string names with a shallow fix
    -- see note FIX
    mkLets staticBindings  -- ++ ["pkgs" (Param selfPkgSym  ==> pkgSet)]
      (mkSym fixSym @@ "pkgs")
  where
    -- | set of all packages
    -- TODO: use true PackageKey mappings, not the flat version
    -- pkgSet = mkNonRecSet (M.elems $ M.mapWithKey binding $ MKM.flattenKeys lf)
    -- -- | the binding of a single package
    -- binding :: PackageKey -> Package -> Binding NExpr
    -- binding pkgKey pkg = packageKeyToIdentifier pkgKey
    --                      $$= mkPackage (name pkgKey) pkg

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
  [ "name"    $= mkStr name'
  , "version" $= mkStr (version pkg)
  , "src"     $= fetchSrc (resolved pkg)
  -- TODO: How to handle optional dependencies?
  , "nodeBuildInputs"
      $= mkList (((mkSym selfPkgSym) !!.) . packageKeyToIdentifier <$>
        (dependencies pkg )) -- <> optionalDependencies pkg))
  ]
  where
    fetchSrc :: RemoteFile -> NExpr
    fetchSrc rf = mkSym fetchurlSym @@ mkNonRecSet
                   [ "url"  $= mkStr (url rf)
                   , "sha1" $= mkStr (sha1sum rf) ]

-- | Representation of a PackageKey as nix attribute name.
packageKeyToIdentifier :: PackageKey -> Text
packageKeyToIdentifier pk = name pk <> "@" <> npmSemver pk


-- hnix helpers
-- TODO maybe submit upstream? Or just own module

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

-- | shortcut to create a list of closed params, like @{ foo, bar, baz }:@
simpleParamSet :: [Text] -> Params NExpr
simpleParamSet = mkParamset . fmap (, Nothing)

-- | shortcut to create a list of multiple params, like @a: b: c:@
multiParam :: [Text] -> NExpr -> NExpr
multiParam ps expr = foldr mkFunction expr $ map Param ps

-- TODO: switch over to $= when
-- https://github.com/jwiegley/hnix/commit/8b4c137a3b125f52bb78039a9d201492032b38e8
-- goes upstream
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
