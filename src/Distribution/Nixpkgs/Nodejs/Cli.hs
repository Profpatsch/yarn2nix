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
  [ "yarn2nix [path/to/yarn.lock]"
  , "yarn2nix --template [path/to/package.json]"
  , ""
  , "Convert a `yarn.lock` into a synonymous nix expression."
  , "If no path is given, search for `./yarn.lock`."
  , "In the second invocation generate a template for your `package.json`."
  ]

data Mode = Yarn | Node
fileFor :: Mode -> Text
fileFor Yarn = "yarn.lock"
fileFor Node = "package.json"

-- | Main entry point for @yarn2nix@.
cli :: [Text] -> IO ()
cli = \case
  ["--help"] -> putText usage
  ("--template":xs) -> fileLogic Node xs
  xs -> fileLogic Yarn xs
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
    parseFile Yarn = parseYarn
    parseFile Node = parseNode
    parseYarn :: FilePath -> IO ()
    parseYarn path = do
      let pathT = toS path
      fc <- readFile path
        `catch` \e
          -> do dieWithUsage ("Unable to open " <> pathT <> ":\n" <> show (e :: IOException))
                pure ""
      case YL.parse path fc of
        Right yarnfile  -> toStdout yarnfile
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
toStdout :: YLT.Lockfile -> IO ()
toStdout lf = do
  ch <- newChan
  -- thrd <- forkIO $ forever $ do
  --   readChan ch >>= \case
  --     FileRemote{..} -> pass
  --     GitRemote{..} -> print $ "Downloading " <> gitRepoUrl
  lf' <- Res.resolveLockfileStatus ch (YLH.decycle lf) >>= \case
    Left err -> die' (T.intercalate "\n" $ toList err)
    Right res -> pure res
  -- killThread thrd
  putDoc $ NixP.prettyNix $ NixOut.mkPackageSet $ NixOut.convertLockfile lf'
