{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, NoImplicitPrelude, LambdaCase, TypeApplications, RecordWildCards, ScopedTypeVariables #-}
module TestNpmjsPackage (tests) where

import Protolude
import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.HUnit (Assertion, testCase)
import qualified Test.Tasty.HUnit as HUnit
-- import Test.Tasty.QuickCheck

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as Text

import qualified Distribution.Nodejs.Package as NP

assertEqual :: (HasCallStack, Eq a, Show a) => Text -> a -> a -> Assertion
assertEqual t a b = HUnit.assertEqual (toS t) a b

assertBool :: (HasCallStack) => Text -> Bool -> Assertion
assertBool t b = HUnit.assertBool (toS t) b

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

formatWarnings :: [NP.Warning] -> Text
formatWarnings ws = Text.intercalate ", " (map f ws)
  where
    f w@(NP.PlainWarning _) = "PlainWarning `" <> NP.formatWarning w <> "`"
    f w@(NP.WrongType {..}) = "WrongType `" <> NP.formatWarning w <> "`"

parseZoom :: (Eq a, Show a)
          => Text -> A.Value -> (NP.Package -> a) -> a
          -> Assertion
parseZoom name got zoom want =
  parseWithWarningsZoom name got zoom want
    (\ws -> assertBool ("unexpected warnings: " <> formatWarnings ws ) $ null ws)

data WarningType
  = SomePlainWarning
  | WrongTypeField
    { wrongTypeField :: Text
    , wrongTypeDefault :: Maybe ()
    }
  deriving (Show)

-- TODO: the warning list should be an exact list/set!
hasWarning :: WarningType -> [NP.Warning] -> Assertion
hasWarning t = assertBool ("no such warning: " <> show t)
               . any (checkWarningType t)

checkWarningType :: WarningType -> NP.Warning -> Bool
checkWarningType tp w = case (tp, w) of
  (SomePlainWarning, NP.PlainWarning _) -> True
  ( WrongTypeField { wrongTypeField = ft
                   , wrongTypeDefault = deft },
    NP.WrongType { NP.wrongTypeField = f
                 , NP.wrongTypeDefault = def })
    -> ft == f && case (deft, def) of
        (Nothing, Nothing) -> True
        (Just (), Just _) -> True
        _ -> False
  (_, _) -> False

case_dependencies :: Assertion
case_dependencies = do
  parseZoom "dependencies are missing"
            (baseAnd [ ])
            NP.dependencies
            mempty

  parseZoom "dependencies are empty"
            (baseAnd [ ("dependencies", A.object []) ])
            NP.dependencies
            mempty

  parseZoom "some dependencies"
            (baseAnd [ ("dependencies", A.object
                       [ ("foo", "1.2.3")
                       , ("bar", "3.4.0") ]) ])
            NP.dependencies
            (HML.fromList
              [ ("foo", "1.2.3")
              , ("bar", "3.4.0") ])

  parseWithWarningsZoom "dependencies are an empty list"
            (baseAnd [ ("dependencies", A.Array mempty) ])
            NP.dependencies
            mempty
            (hasWarning $ WrongTypeField
              { wrongTypeField = "dependencies"
              , wrongTypeDefault = Just () })

  parseWithWarningsZoom "dependencies is a random scalar"
            (baseAnd [ ("dependencies", A.String "hiho") ])
            NP.dependencies
            mempty
            (hasWarning $ WrongTypeField
              { wrongTypeField = "dependencies"
              , wrongTypeDefault = Just () })

  parseFailure (Proxy @NP.LoggingPackage) "dependencies are a non-empty list"
            (baseAnd [ ("dependencies", A.Array (pure "foo")) ])

case_binPaths :: Assertion
case_binPaths = do
  parseZoom ".bin exists with files"
            (baseAnd [ ("bin", "./abc") ])
            NP.bin
            (NP.BinFiles $ HML.fromList [ ("foopkg", "./abc") ])

  parseZoom "scoped package"
            (baseAnd [ ("name", "@foo/bar"), ("bin", "./abc") ])
            NP.bin
            (NP.BinFiles $ HML.fromList [ ("bar", "./abc") ])

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
                        (hasWarning (WrongTypeField
                           { wrongTypeField = "scripts.foo"
                           , wrongTypeDefault = Nothing }))

parseSuccess :: (A.FromJSON a) => A.Value -> IO a
parseSuccess v = case A.fromJSON v of
  (AT.Error err) -> HUnit.assertFailure err >> panic "not reached"
  (AT.Success a) -> pure a

parseFailure :: forall a. (A.FromJSON a) => Proxy a -> Text -> A.Value -> IO ()
parseFailure Proxy msg v = case AT.fromJSON @a v of
  -- TODO: check the error?
  (AT.Error _) -> pass
  (AT.Success _) -> HUnit.assertFailure $ (toS msg) <> ", parse should have failed."

tests :: TestTree
tests = $(testGroupGenerator)
