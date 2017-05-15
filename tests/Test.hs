module Main where

import Test.Tasty
import qualified TestLock as TL

main = defaultMain TL.tests
