module Main where

import Test.Tasty
import qualified TestNpmjsPackage as Npmjs

main :: IO ()
main = defaultMain $ testGroup "tests"
  [ Npmjs.tests
  ]
