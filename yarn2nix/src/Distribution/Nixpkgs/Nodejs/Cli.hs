{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-|
Description: command line interface
-}
module Distribution.Nixpkgs.Nodejs.Cli
( cli
, parseArgsPure
)
where

import Protolude
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified Options.Applicative.Help.Pretty as O (linebreak)
import qualified System.Directory as Dir
import System.Environment (getProgName)

import qualified Nix.Pretty as NixP
import qualified Prettyprinter.Render.Text as RT
import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YLT
import qualified Yarn.Lock.Helpers as YLH

import qualified Distribution.Nixpkgs.Nodejs.OptimizedNixOutput as NixOut
import qualified Distribution.Nixpkgs.Nodejs.FromPackage as NodeFP
import qualified Distribution.Nixpkgs.Nodejs.ResolveLockfile as Res
import qualified Distribution.Nodejs.Package as NP
import Distribution.Nixpkgs.Nodejs.ResolveLockfile (ResolverConfig(ResolverConfig, resolveOffline))
import Distribution.Nixpkgs.Nodejs.License (LicensesBySpdxId)
import qualified Data.Aeson as Json


description :: O.InfoMod a
description = O.fullDesc
  <> O.progDescDoc (Just $ mconcat $ intersperse O.linebreak
   [ "yarn2nix has two modes:"
   <> O.linebreak
   , "In its default mode (started without --template) it parses a given yarn.lock file"
   , "and prints a nix expressions representing it to stdout."
   <> O.linebreak
   , "If --template is given, it processes a given package.json"
   , "and prints a template nix expression for an equivalent nix package."
   <> O.linebreak
   , "In both modes yarn2nix will take the file as an argument"
   , "or read it from stdin if it is missing."
   ])

-- | Main entry point for @yarn2nix@.
cli :: IO ()
cli = parseArgs >>= runAction

-- | Type of action @yarn2nix@ is performing.
data RunMode
  = YarnLock     -- ^ Output a nix expression for a @yarn.lock@
  | NodeTemplate -- ^ Output a nix template corresponding to a @package.json@
  deriving (Show, Eq)

-- | Runtime configuration of @yarn2nix@. Typically this is determined from
--   its command line arguments and valid for the current invocation only.
data RunConfig
  = RunConfig
  { runMode         :: RunMode
  , runOffline      :: Bool            -- ^ If @True@, @yarn2nix@ will fail if it
                                       --   requires network access. Currently this means
                                       --   'Distribution.Nixpkgs.Nodejs.ResolveLockfile.resolveLockfileStatus'
                                       --   will throw an error in case resolving a hash
                                       --   requires network access.
  , runLicensesJson :: Maybe FilePath  -- ^ Optional Path to a licenses.json file
                                       --   equivalent to the lib.licenses set from
                                       --   @nixpkgs@.
  , runInputFile    :: Maybe FilePath  -- ^ File to process. If missing the appropriate
                                       --   file for the current mode from the current
                                       --   working directory is used.
  } deriving (Show, Eq)


fileFor :: RunConfig -> Text
fileFor cfg =
  case runMode cfg of
    YarnLock -> "yarn.lock"
    NodeTemplate -> "package.json"

parseArgs :: IO RunConfig
parseArgs = O.customExecParser optparsePrefs runConfigParserWithHelp

parseArgsPure :: [Text] -> IO RunConfig
parseArgsPure args =
  args
  <&> T.unpack
  & O.execParserPure optparsePrefs runConfigParserWithHelp
  & O.handleParseResult


runAction :: RunConfig -> IO ()
runAction cfg = do
  file <- fileForConfig
  case runMode cfg of
    YarnLock -> parseYarn file
    NodeTemplate -> parseNode file
  where
    fileForConfig :: IO FilePath
    fileForConfig =
      case runInputFile cfg of
        Just f -> pure f
        Nothing -> Dir.getCurrentDirectory >>= \d ->
          Dir.findFile [d] (toS $ fileFor cfg) >>= \case
            Nothing -> dieWithUsage
              $ "No " <> fileFor cfg <> " found in current directory"
            Just path -> pure path
    parseYarn :: FilePath -> IO ()
    parseYarn path = do
      fc <- catchCouldNotOpen path $ readFile path
      case YL.parse path fc of
        Right yarnfile  -> toStdout cfg yarnfile
        Left err -> die' ("Could not parse " <> toS path <> ":\n" <> YL.prettyLockfileError err)

    parseNode :: FilePath -> IO ()
    parseNode path = do
      BL.readFile path >>= (\case
        Right (NP.LoggingPackage (nodeModule, warnings)) -> do
          for_ warnings $ TIO.hPutStrLn stderr . NP.formatWarning

          licenseSet <- case cfg & runLicensesJson of
            Nothing -> pure Nothing
            Just licensesJson -> do
              catchCouldNotOpen licensesJson
                (BL.readFile licensesJson)
                <&> Json.decode @LicensesBySpdxId

          print $ NixP.prettyNix $ NodeFP.genTemplate licenseSet nodeModule
        Left err -> die' ("Could not parse " <> toS path <> ":\n" <> show err)) . NP.decode
    catchCouldNotOpen :: FilePath -> IO a -> IO a
    catchCouldNotOpen path action = action `catch` \e ->
      dieWithUsage $ "Could not open " <> toS path <> ":\n" <> show (e :: IOException)

-- get rid of odd linebreaks by increasing width enough
optparsePrefs :: O.ParserPrefs
optparsePrefs = O.defaultPrefs { O.prefColumns = 100 }

-- If --template is given, run in NodeTemplate mode,
-- otherwise the default mode YarnLock is used.
runModeParser :: O.Parser RunMode
runModeParser = O.flag YarnLock NodeTemplate $
     O.long "template"
  <> O.help "Output a nix package template for a given package.json"

runConfigParser :: O.Parser RunConfig
runConfigParser = RunConfig
  <$> runModeParser
  <*> O.switch
      (O.long "offline"
    <> O.help "Makes yarn2nix fail if network access is required")
  <*> O.optional (O.option O.str
     (O.long "license-data"
   <> O.metavar "FILE"
   <> O.help "Path to a license.json equivalent to nixpkgs.lib.licenses"
   -- only really interesting for wrapping at build
   <> O.internal))
  <*> O.optional (O.argument O.str (O.metavar "FILE"))

runConfigParserWithHelp :: O.ParserInfo RunConfig
runConfigParserWithHelp =
  O.info (runConfigParser <**> O.helper) description

die' :: Text -> IO a
die' err = putErrText err *> exitFailure

dieWithUsage :: Text -> IO a
dieWithUsage err = do
  putErrText (err <> "\n")
  progn <- getProgName
  hPutStr stderr
    . fst . flip O.renderFailure progn
    $ O.parserFailure optparsePrefs
        runConfigParserWithHelp (O.ShowHelpText Nothing) mempty
  exitFailure

-- TODO refactor
toStdout :: RunConfig -> YLT.Lockfile -> IO ()
toStdout cfg lf = do
  ch <- newChan
  -- thrd <- forkIO $ forever $ do
  --   readChan ch >>= \case
  --     FileRemote{..} -> pass
  --     GitRemote{..} -> print $ "Downloading " <> gitRepoUrl
  let resolverConfig = ResolverConfig {
    resolveOffline = cfg & runOffline
   }
  lf' <- Res.resolveLockfileStatus resolverConfig ch (YLH.decycle lf) >>= \case
    Left err -> die' (T.intercalate "\n" $ toList err)
    Right res -> pure res
  -- killThread thrd
  RT.putDoc $ NixP.prettyNix $ NixOut.mkPackageSet $ NixOut.convertLockfile lf'
