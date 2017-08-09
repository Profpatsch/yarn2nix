module Main where

import Test.Tasty
import qualified TestParse as TP

main :: IO ()
main = defaultMain TP.tests
