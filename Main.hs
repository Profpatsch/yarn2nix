{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Main where

import Protolude
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import qualified System.Directory as Dir

import qualified Nix.Pretty as NixP
import qualified Yarn.Lock as YL

import qualified Distribution.Nixpkgs.Nodejs.FromYarnLock as FYL
import qualified Distribution.Nixpkgs.Nodejs.FromPackage as FP

data Mode = Yarn | Node
fileFor :: Mode -> Text
fileFor Yarn = "yarn.lock"
fileFor Node = "package.json"

main :: IO ()
main = do
  getArgs >>= \case
    ["--help"] -> putText usage
    ("--template":xs) -> fileLogic Node xs
    xs -> fileLogic Yarn xs
  where
    fileLogic :: Mode -> [[Char]] -> IO ()
    fileLogic mode = \case
      [] -> Dir.getCurrentDirectory >>= \d ->
          Dir.findFile [d] (toS $ fileFor mode) >>= \case
            Nothing -> do
              dieWithUsage $ "no " <> fileFor mode <> " found in current directory"
            Just path  -> parseFile mode path
      [path] -> parseFile mode path
      _ -> dieWithUsage ""
    parseFile :: Mode -> FilePath -> IO ()
    parseFile Yarn = parseYarn
    parseFile Node = parseNode
    parseYarn :: FilePath -> IO ()
    parseYarn path = do
      let path' = toS path
      fc <- readFile path
        `catch` \e
          -> do dieWithUsage ("unable to open " <> path' <> ":\n" <> show (e :: IOException))
                pure ""
      case YL.parse path' fc of
        Right yarnfile  -> FYL.toStdout yarnfile
        Left err -> die' ("could not parse " <> path' <> ":\n" <> show err)
    parseNode :: FilePath -> IO ()
    parseNode path = do
      FP.decode <$> BL.readFile path >>= \case
        Right nodeModule -> print $ NixP.prettyNix $ FP.genTemplate nodeModule
        Left err -> die' ("could not parse " <> toS path <> ":\n" <> show err)

die' :: Text -> IO ()
die' err = putText err *> exitFailure
dieWithUsage :: Text -> IO ()
dieWithUsage err = die' (err <> "\n" <> usage)


usage :: Text
usage = mconcat $ intersperse "\n"
  [ "yarn2nix [path/to/yarn.lock]"
  , "yarn2nix --template [path/to/package.json]"
  , ""
  , "Convert a `yarn.lock` into a synonymous nix expression."
  , "If no path is given, search for `./yarn.lock`."
  , "In the second invocation generate a template for your `package.json`."
  ]
