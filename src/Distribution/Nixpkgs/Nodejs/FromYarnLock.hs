{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, ViewPatterns, RecordWildCards, NoImplicitPrelude, LambdaCase, NamedFieldPuns, GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Distribution.Nixpkgs.Nodejs.FromYarnLock
(
-- ( toStdout
-- , mkPackageSet
resolveLockfile
) where

import Protolude
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import qualified Control.Monad.Trans.Either as E
import Data.Foldable (foldr1)
import Data.Fix (Fix(Fix))
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.MultiKeyedMap as MKM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT
import qualified System.Process as Process

import Nix.Expr
import Nix.Expr.Additions
import Nix.Pretty (prettyNix)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Control.Concurrent.Async.Pool as Async

import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YLT
import qualified Yarn.Lock.Helpers as YLH
import Distribution.Nixpkgs.Nodejs.Utils

-- | A thing whose hash is already known (“resolved”).
--
-- Only packages with known hashes are truly “locked”.
data Resolved a = Resolved
  { sha1sum :: Text
  , resolved :: a
  } deriving (Show, Eq, Functor)

-- | In order to write a nix file, all packages need to know their shasums first.
type ResolvedLockfile = MKM.MKMap YLT.PackageKey (Resolved YLT.Package)

-- | Resolve all packages by downloading their sources if necessary.
resolveLockfile :: YLT.Lockfile -> IO ResolvedLockfile
resolveLockfile = traverse (\(YLT.Package{..}) ->
    resolve remote >>= \case
      (Left err) -> panic err
      (Right resRemote) -> do
        case resRemote of
          (Resolved _ r@YLT.GitRemote{..}) -> print r
          _ -> pass
        pure $ fmap (\remote -> YLT.Package{..}) resRemote)
  where
    resolve :: YLT.Remote -> IO (Either Text (Resolved YLT.Remote))
    resolve = E.runEitherT . \case
      (resolved@YLT.FileRemote{..}) ->
        pure $ Resolved{ sha1sum = fileSha1, .. }
      (resolved@YLT.GitRemote{..}) -> do
        sha1sum <- fetchFromGit gitRepoUrl gitRev
        pure $ Resolved{..}

    fetchFromGit :: Text -> Text -> E.EitherT Text IO Text
    fetchFromGit repo rev = do
      out <- liftIO $ Process.readProcessWithExitCode "nix-prefetch-git"
               ["--url", toS repo, "--rev", toS rev, "--hash", "sha1"] ""
      case out of
        ((ExitFailure _), _, err) -> E.left $ toS err
        (ExitSuccess, out, _) -> E.hoistEither
          $ first (\decErr -> "parsing json output failed:\n"
                    <> toS decErr <> "\nThe output was:\n" <> toS out)
            $ do val <- Aeson.eitherDecode' (toS out)
                 AesonT.parseEither
                   (Aeson.withObject "PrefetchOutput" (Aeson..: "sha1")) val



{-
-- | Pretty print the nixpkgs version of @yarn.lock@ to stdout.
toStdout  :: YLT.Lockfile -> IO ()
toStdout lf = do
  let (res, warnings) = runWriter $ mkPackageSet lf
  PP.putDoc . foldr1 (PP.<$$>)
    $ map (PP.text . toS . ("Warning: "<>) . unWarn) warnings
  PP.putDoc . prettyNix $ res
-}

-- CONVENTION: *Sym is a nix symbol
-- We keep them Text, because that’s what they are in hnix
-- TODO: find a way to actually have a nice type for symbols

-- | The name of the function to build node packages.
buildNodePackageSym :: Text
buildNodePackageSym = "buildNodePackage"

-- | The name of the self attribute.
selfPkgSym :: Text
selfPkgSym = "s"

fixSym, fetchurlSym :: Text
-- | The name of the fix function.
fixSym = "fix"
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
  , registryShortSym :: ShortSym
  -- ^ shortened symbol name
  , registryPrefix :: Text
  -- ^ prefix string that selects to this prefix
  , buildRegistry :: AntiQ -> AntiQ -> [RegVal]
  -- ^ takes name and version, interpolates them
  }

-- | Shortened alias for a symbol (to bring down filesize)
newtype ShortSym = ShortSym { unShortSym :: Text } deriving (IsString)

yarnRegistry :: Registry
yarnRegistry = Registry "yarnpkg" "y" p
                $ \n v -> [T p, V n, T "/-/", V n, T "-", V v, T ".tgz"]
  where p = "https://registry.yarnpkg.com/"

-- | all Registries with their shortened names
allRegistries :: [Registry]
allRegistries = [yarnRegistry]

recognizeRegistry :: Text -> Maybe Registry
recognizeRegistry t = listToMaybe
  $ filter (\Registry{..} -> registryPrefix `T.isPrefixOf` t)
  $ allRegistries

prefixesSym :: Text
prefixesSym = "prefixes"

-- | combine a binding and its short symbol name
data ShortenedBinding = ShortenedBinding
  { bindingSym :: Text
  , bindingShortSym :: ShortSym
  , binding :: NExpr }

-- | helper to get the short sym out as 'NExpr'
shortSym :: ShortenedBinding -> NExpr
shortSym = mkSym . unShortSym . bindingShortSym

pkgBuildFn :: ShortenedBinding
pkgBuildFn = ShortenedBinding "shortBuildPkg" "b" bnd
  where
    bnd = multiParam ["name", "version", "prfx", "sha1", "deps"]
            $ mkSym buildNodePackageSym @@ mkNonRecSet
              [ inherit $ map StaticKey ["name", "version"]
              , "src" $= (mkSym fetchurlSym @@ mkNonRecSet
                [ "url" $= ("prfx" @@ "name" @@ "version")
                , inherit $ [StaticKey "sha1"] ])
              , "nodeBuildInputs" $= "deps"
              ]

allShortenedBindings  :: [ShortenedBinding]
allShortenedBindings = [pkgBuildFn]

registryShortenedBinding :: Registry -> ShortenedBinding
registryShortenedBinding Registry{registrySym, registryShortSym, buildRegistry} =
  ShortenedBinding registrySym registryShortSym $ multiParam ["n", "v"]
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
      (mkSymBnds $ map registryShortenedBinding allRegistries)
    -- list of shortened definitions
    shortenedDefinitions = mkShortSymBnds $
      (map (ln Nothing)             allShortenedBindings) ++
      (map (ln (Just prefixesSym) . registryShortenedBinding) allRegistries)
    -- link to the full definition that has maybe a prefix
    ln :: Maybe Text -> ShortenedBinding -> ShortenedBinding
    ln expr sb@ShortenedBinding{bindingSym} =
      let replRHS new = sb { binding = new } in
        case expr of
          Nothing  -> replRHS $ mkSym bindingSym
          Just prf -> replRHS $ mkSym prf !!. bindingSym

-- TODO: improve
newtype Warning = Warn { unWarn :: Text }

{-
-- | Convert a @yarn.lock@ to a nix expression.
mkPackageSet :: YLT.Lockfile -> Writer [Warning] NExpr
mkPackageSet (YLH.decycle -> lf) = do
  pkgMap <- sequence $ M.mapWithKey recognizeOrWarn
    -- TODO: use true PackageKey mappings, not the flat version
           $ MKM.flattenKeys lf

  pure $ simpleParamSet [fixSym, fetchurlSym] ==> Param buildNodePackageSym ==>
    -- enable self-referencing of packages
    -- with string names with a shallow fix
    -- see note FIX
    mkLets (staticBindings ++ ["pkgs" $= (Param selfPkgSym  ==> pkgSet pkgMap)])
      (mkSym fixSym @@ "pkgs")
  where
    -- | set of all package
    recognizeOrWarn :: YLT.PackageKey -> YLT.Package -> Writer [Warning] NExpr
    recognizeOrWarn key pkg = case recognizeRegistry regStr of
         Nothing -> tell [tmpWarn] >> pure (mk $mkStr regStr)
         Just reg -> pure $ mk (shortSym $ registryShortenedBinding reg)
      where
        mk x = mkPackage (YLT.name key) x pkg
        regStr = url $ resolved pkg
        tmpWarn :: Warning
        tmpWarn = Warn $ "Package `" <> packageKeyToIdentifier key
               <> "` contains unknown registry, " <> url (resolved pkg)
    pkgSet :: Map YLT.PackageKey NExpr -> NExpr
    pkgSet = mkNonRecSet . M.elems . M.mapWithKey
               (\key val -> packageKeyToIdentifier key $$= val)

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
mkPackage :: Text -> NExpr -> YLT.Package -> NExpr
mkPackage name' registry (YLT.Package ver remote deps optdeps) = (shortSym pkgBuildFn)
  @@ mkStr name' @@ mkStr ver @@ registry
  @@ mkStr (sha1sum remote) @@ depList
  where
    -- TODO: How to handle optional dependencies?
    depList = mkList (((mkSym selfPkgSym) !!.)
                      . packageKeyToIdentifier <$> (deps <> optdeps))


-}
