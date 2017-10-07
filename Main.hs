module Main where

import Distribution.Nixpkgs.Nodejs.Cli (cli)
import qualified Data.Text as T
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= cli . map T.pack


