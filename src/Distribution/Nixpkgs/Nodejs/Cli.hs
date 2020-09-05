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
import Distribution.Nixpkgs.Nodejs.RunConfig

fileFor :: RunConfig -> Text
fileFor cfg =
  case runMode cfg of
    YarnLock -> "yarn.lock"
    NodeTemplate -> "package.json"

-- | Main entry point for @yarn2nix@.
cli :: RunConfig -> IO ()
cli cfg = do
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
            Nothing -> die'
              $ "No " <> fileFor cfg <> " found in current directory"
            Just path -> pure path
    parseYarn :: FilePath -> IO ()
    parseYarn path = do
      let pathT = toS path
      fc <- readFile path
        `catch` \e
          -> die' ("Unable to open " <> pathT <> ":\n" <> show (e :: IOException))
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

-- TODO refactor
toStdout :: RunConfig -> YLT.Lockfile -> IO ()
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
