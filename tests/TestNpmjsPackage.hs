{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, NoImplicitPrelude, LambdaCase #-}
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
  let
    parseWithWarningsZoom :: (Eq a, Show a)
                          => Text -> A.Value -> (NP.Package -> a) -> a
                          -> ([NP.Warning] -> Assertion)
                          -> Assertion
    parseWithWarningsZoom name got zoom want warningPred =
      NP.unLoggingPackage <$> parseSuccess got
      >>= \(val, warnings) -> do
              assertEqual (toS name) want (zoom val)
              warningPred warnings
    parseZoom name got zoom want =
      parseWithWarningsZoom name got zoom want (const $ pure ())

    hasWarning :: (NP.Warning -> Bool) -> [NP.Warning] -> Assertion
    hasWarning warningPred = assertBool "no such warning!" . any warningPred
    wrongType field def = \case
      (NP.WrongType f d) -> field == f && def == d
      _ -> False
    plainWarning = \case
      (NP.PlainWarning _) -> True
      _ -> False

  parseZoom ".bin exists with files"
            (baseAnd [ ("bin", "./abc") ])
            NP.bin
            (NP.BinFiles $ HML.fromList [ ("foopkg", "./abc") ])

  parseZoom ".directories.bin exists with path"
            (baseAnd [ ("directories", A.object [("bin", "./abc")]) ])
            NP.bin
            (NP.BinFolder "./abc")

  parseZoom "multiple .bin files are parsed"
            (baseAnd [ ("bin", A.object
                       [ ("one", "./bin/one")
                       , ("two", "imhere") ]) ])
            NP.bin
            (NP.BinFiles $ HML.fromList
              [ ("one", "./bin/one")
              , ("two", "imhere") ])

  parseWithWarningsZoom "bin and directories.bin both exist"
                        (baseAnd [ ("bin", "foo")
                                 , ("directories", A.object
                                     [ ("bin", "foo") ]) ])
                        NP.bin
                        (NP.BinFiles mempty)
                        (hasWarning plainWarning)

  parseZoom "neither .bin nor .directories.bin exis"
            (baseAnd [])
            NP.bin
            (NP.BinFiles mempty)

  parseWithWarningsZoom ".scripts field has a wrong type"
                        (baseAnd [ ("scripts", A.object
                                   [ ("foo", A.object [])
                                   , ("bar", "imascript") ]) ])
                        NP.scripts
                        (HML.fromList [ ("bar", "imascript") ])
                        (hasWarning (wrongType "scripts.foo" Nothing))

parseSuccess :: (A.FromJSON a) => A.Value -> IO a
parseSuccess v = case A.fromJSON v of
  (AT.Error err) -> assertFailure err >> panic "not reached"
  (AT.Success a) -> pure a
  
-- parseFailure :: Show a => (A.Value -> AT.Parser a) -> A.Value -> IO ()
-- parseFailure p v = case AT.parse p v of
--   (AT.Error _) -> pass
--   (AT.Success a) -> assertFailure $ "no parse" <> show a

tests :: TestTree
tests = $(testGroupGenerator)
