module Main where

import Test.Tasty
import qualified TestParse as Parse
import qualified TestFile as File
import qualified TestMultiKeyedMap as MKM

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Parse.tests
  , File.tests
  , MKM.tests
  ]
