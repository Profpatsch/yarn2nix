{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, NoImplicitPrelude #-}
module TestNpmjsPackage (tests) where

import Protolude
import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Lazy as HML

import qualified Distribution.Nodejs.Package as NP

baseAnd :: [(Text, A.Value)] -> A.Value
baseAnd fields = A.Object $ HML.fromList $
  [ ("name", "foopkg")
  , ("version", "1.0.2")
  ] <> fields

case_binPaths :: Assertion
case_binPaths = do
  parseSuccess bin (baseAnd [ ("bin", "./abc") ])
    >>= assertEqual "bin path" (NP.BinFiles $ HML.fromList
                           [ ("foopkg", "./abc") ])
  parseSuccess bin (baseAnd [ ("directories", A.object [("bin", "./abc")]) ])
    >>= assertEqual "bin path" (NP.BinFolder "./abc")
  parseSuccess bin (baseAnd [ ("bin", A.object
                           [ ("one", "./bin/one")
                           , ("two", "imhere") ]) ])
    >>= assertEqual "bin multiple" (NP.BinFiles $ HML.fromList
                           [ ("one", "./bin/one")
                           , ("two", "imhere") ])
  parseFailure bin (baseAnd [ ("bin", "foo")
                            , ("directories", A.object [("bin", "foo")]) ])
  parseSuccess bin (baseAnd [])
    >>= assertEqual "no bins" (NP.BinFiles mempty)
  where
    bin = fmap NP.bin . A.parseJSON

parseSuccess :: (A.Value -> AT.Parser a) -> A.Value -> IO a
parseSuccess p v = case AT.parse p v of
  (AT.Error err) -> assertFailure err >> panic "not reached"
  (AT.Success a) -> pure a
  
parseFailure :: Show a => (A.Value -> AT.Parser a) -> A.Value -> IO ()
parseFailure p v = case AT.parse p v of
  (AT.Error _) -> pass
  (AT.Success a) -> assertFailure $ "no parse" <> show a

tests :: TestTree
tests = $(testGroupGenerator)
