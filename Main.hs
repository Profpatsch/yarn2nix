{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude
import Distribution.Nixpkgs.Nodejs.Cli (cli)
import Distribution.Nixpkgs.Nodejs.RunConfig
import Options.Applicative
import Options.Applicative.Help.Pretty (linebreak)

main :: IO ()
main = execParser (info (runConfigParser <**> helper) desc) >>= cli

desc :: InfoMod a
desc = fullDesc
  <> progDescDoc (Just $ mconcat $ intersperse linebreak
   [ "yarn2nix has two modes:"
   <> linebreak
   , "In its default mode (started without --template) it parses a given yarn.lock file"
   , "and prints a nix expressions representing it to stdout."
   <> linebreak
   , "If --template is given, it processes a given package.json"
   , "and prints a template nix expression for an equivalent nix package."
   <> linebreak
   , "In both modes yarn2nix will take the file as an argument"
   , "or read it from stdin if it is missing."
   ])

-- If --template is given, run in NodeTemplate mode,
-- otherwise the default mode YarnLock is used.
runModeParser :: Parser RunMode
runModeParser = flag YarnLock NodeTemplate $
     long "template"
  <> help "Output a nix package template for a given package.json"

runConfigParser :: Parser RunConfig
runConfigParser = RunConfig
  <$> runModeParser
  <*> switch
      (long "offline"
    <> help "Makes yarn2nix fail if network access is required")
  <*> optional (argument str (metavar "FILE"))
