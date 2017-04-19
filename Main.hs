{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Main where

import Protolude
import qualified Distribution.Nixpkgs.Nodejs.FromYarnLock as FYL
import qualified Yarn.Lock as YL
import System.Environment (getArgs)
import qualified System.Directory as Dir

main :: IO ()
main = do
  getArgs >>= \case
    [] -> Dir.getCurrentDirectory >>= \d ->
          Dir.findFile [d] "yarn.lock" >>= \case
            Nothing -> do
              dieWithUsage "no `yarn.lock` found in current directory"
            Just path  -> go path
    ["--help"] -> putText usage
    [path] -> go path
    _ -> dieWithUsage ""
  where
    go :: FilePath -> IO ()
    go path = do
      let path' = toS path
      fc <- readFile path
        `catch` \e
          -> do dieWithUsage ("unable to open " <> path' <> ":\n" <> show (e :: IOException))
                pure ""
      case YL.parse path' fc of
        Right yarnfile  -> FYL.toStdout yarnfile
        Left err -> die' ("could not parse " <> path' <> ":\n" <> show err)

die' :: Text -> IO ()
die' err = putText err *> exitFailure
dieWithUsage :: Text -> IO ()
dieWithUsage err = die' (err <> "\n" <>usage)


usage :: Text
usage = mconcat
  [ "yarn2nix [path/to/yarn.lock]\n"
  , "\n"
  , "Convert a `yarn.lock` into a syononymous nix expression.\n"
  , "If no path is given, search for `./yarn.lock`."
  ]
