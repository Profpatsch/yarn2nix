module Main where

import Test.Tasty
import qualified TestParse as Parse
import qualified TestFile as File

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Parse.tests
  , File.tests
  ]
