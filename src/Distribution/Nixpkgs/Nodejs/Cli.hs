{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}
{-|
Description: command line interface
-}
module Distribution.Nixpkgs.Nodejs.Cli
( cli
)
where

import Protolude
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir

import qualified Nix.Pretty as NixP
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Yarn.Lock as YL
import qualified Yarn.Lock.Types as YLT
import qualified Yarn.Lock.Helpers as YLH

import qualified Distribution.Nixpkgs.Nodejs.OptimizedNixOutput as NixOut
import qualified Distribution.Nixpkgs.Nodejs.FromPackage as NodeFP
import qualified Distribution.Nixpkgs.Nodejs.ResolveLockfile as Res
import qualified Distribution.Nodejs.Package as NP

usage :: Text
usage = mconcat $ intersperse "\n"
  [ "yarn2nix [--offline] [path/to/yarn.lock]"
  , ""
  , "  Convert a `yarn.lock` into a synonymous nix expression."
  , "  If no path is given, search for `./yarn.lock`."
  , "  If --offline is given, abort if figuring out a hash"
  , "  requires network access."
  , ""
  , "yarn2nix --template [path/to/package.json]"
  , ""
  , "  Generate a package template nix-expression for your `package.json`."
  ]

data Mode
  = Node
  | Yarn Res.ResolverConfig

fileFor :: Mode -> Text
fileFor (Yarn _) = "yarn.lock"
fileFor Node = "package.json"

-- | Main entry point for @yarn2nix@.
cli :: [Text] -> IO ()
cli = \case
  ["--help"] -> putText usage
  ("--template":xs) -> fileLogic Node xs
  ("--offline":xs) ->
    fileLogic (Yarn (Res.defResolverConfig { Res.resolveOffline = True })) xs
  xs -> fileLogic (Yarn Res.defResolverConfig) xs
  where
    fileLogic :: Mode -> [Text] -> IO ()
    fileLogic mode = \case
      [] -> Dir.getCurrentDirectory >>= \d ->
          Dir.findFile [d] (toS $ fileFor mode) >>= \case
            Nothing -> do
              dieWithUsage $ "No " <> fileFor mode <> " found in current directory"
            Just path  -> parseFile mode path
      [path] -> parseFile mode (toS path)
      _ -> dieWithUsage ""
    parseFile :: Mode -> FilePath -> IO ()
    parseFile (Yarn cfg) = parseYarn cfg
    parseFile Node = parseNode
    parseYarn :: Res.ResolverConfig -> FilePath -> IO ()
    parseYarn cfg path = do
      let pathT = toS path
      fc <- readFile path
        `catch` \e
          -> do dieWithUsage ("Unable to open " <> pathT <> ":\n" <> show (e :: IOException))
                pure ""
      case YL.parse path fc of
        Right yarnfile  -> toStdout cfg yarnfile
        Left err -> die' ("Could not parse " <> pathT <> ":\n" <> show err)
    parseNode :: FilePath -> IO ()
    parseNode path = do
      NP.decode <$> BL.readFile path >>= \case
        Right (NP.LoggingPackage (nodeModule, warnings)) -> do
          for_ warnings $ TIO.hPutStrLn stderr . NP.formatWarning
          print $ NixP.prettyNix $ NodeFP.genTemplate nodeModule
        Left err -> die' ("Could not parse " <> toS path <> ":\n" <> show err)

die' :: Text -> IO a
die' err = putErrText err *> exitFailure
dieWithUsage :: Text -> IO ()
dieWithUsage err = die' (err <> "\n" <> usage)


-- TODO refactor
toStdout :: Res.ResolverConfig -> YLT.Lockfile -> IO ()
toStdout cfg lf = do
  ch <- newChan
  -- thrd <- forkIO $ forever $ do
  --   readChan ch >>= \case
  --     FileRemote{..} -> pass
  --     GitRemote{..} -> print $ "Downloading " <> gitRepoUrl
  lf' <- Res.resolveLockfileStatus cfg ch (YLH.decycle lf) >>= \case
    Left err -> die' (T.intercalate "\n" $ toList err)
    Right res -> pure res
  -- killThread thrd
  putDoc $ NixP.prettyNix $ NixOut.mkPackageSet $ NixOut.convertLockfile lf'
