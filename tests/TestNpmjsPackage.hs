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

parseWithWarningsZoom :: (Eq a, Show a)
                      => Text -> A.Value -> (NP.Package -> a) -> a
                      -> ([NP.Warning] -> Assertion)
                      -> Assertion
parseWithWarningsZoom name got zoom want warningPred =
  NP.unLoggingPackage <$> parseSuccess got
  >>= \(val, warnings) -> do
          assertEqual (toS name) want (zoom val)
          warningPred warnings

parseZoom :: (Eq a, Show a)
          => Text -> A.Value -> (NP.Package -> a) -> a
          -> Assertion
parseZoom name got zoom want =
  parseWithWarningsZoom name got zoom want (const $ pure ())

data WarningType
  = SomePlainWarning
  | ExactWrongType
    { wrongTypeField :: Text
    , wrongTypeDefault :: Maybe Text
    }
  deriving (Show)

hasWarning :: WarningType -> [NP.Warning] -> Assertion
hasWarning t = assertBool ("no such warning: " <> show t)
               . any (checkWarningType t)

checkWarningType :: WarningType -> NP.Warning -> Bool
checkWarningType tp w = case (tp, w) of
  (SomePlainWarning, NP.PlainWarning _) -> True
  ( ExactWrongType { wrongTypeField = ft
                   , wrongTypeDefault = deft },
    NP.WrongType { NP.wrongTypeField = f
                 , NP.wrongTypeDefault = def })
    -> ft == f && deft == def
  (_, _) -> False

case_binPaths :: Assertion
case_binPaths = do
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
                        (hasWarning SomePlainWarning)

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
                        (hasWarning (ExactWrongType "scripts.foo" Nothing))

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
