{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, NamedFieldPuns, ViewPatterns, NoImplicitPrelude #-}
module TestParse (tests) where

import Protolude
import Data.MultiKeyedMap hiding (keys)
import qualified Data.Map as Map
import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.HUnit
import NeatInterpolation
import qualified Text.Megaparsec as MP
import qualified Data.Char as Ch

import Yarn.Lock.Types
import Yarn.Lock.Parse

-- registryPackage :: Text
-- registryPackage = [text|
--   accepts@1.3.3, accepts@~1.3.3:
--     version "1.3.3"
--     resolved "https://registry.yarnpkg.com/accepts/-/accepts-1.3.3.tgz#c3ca7434938648c3e0d9c1e328dd68b622c284ca"
--     dependencies:
--       mime-types "~2.1.11"
--       negotiator "0.6.1"
--   |]

nonsenseEntry :: Text
nonsenseEntry = [text|
  foobar@~1.2.3, xyz@hehe:
    field1 "°§ℓ»«UAIERNT"
    field2 "nopedidope"
  |]

case_NonsenseASTPackageEntry :: Assertion
case_NonsenseASTPackageEntry = do
  parseSuccess packageEntry nonsenseEntry
    >>= \(Keyed keys (PackageFields fields)) -> do
      assertBool "two keys" (length keys == 2)
      assertBool "two fields" (length fields == 2)
      assertBool "field1 member" (Map.member "field1" fields)
      assertBool "field2 member" (Map.member "field2" fields)
      Map.lookup "field1" fields @=? (Just (Left "°§ℓ»«UAIERNT"))

nestedPackage :: Text
nestedPackage = [text|
  readable-stream@1.0, "readable-stream@>=1.0.33-1 <1.1.0-0":
    dependencies:
      core-util-is "~1.0.0"
      is.array ""
      string_decoder "~0.10.x"
      johnny-dep 2.3.4
  |]

case_NestedPackage :: Assertion
case_NestedPackage = do
  assertBool "there is unicode" (all Ch.isAscii (toS nestedPackage :: [Char]))
  parseSuccess packageEntry nestedPackage
    >>= \(Keyed _ (PackageFields fields)) -> do
      case Map.lookup "dependencies" fields of
        (Nothing) -> assertFailure "where’s the key"
        (Just (Left s)) -> do
          assertFailure $ toS (s <> "should be a nested package")
        (Just (Right (PackageFields nested))) -> do
          assertEqual "nested keys" 4 $ length nested
          assertEqual "dep exists" (Just (Left "2.3.4"))
            $ Map.lookup "johnny-dep" nested 

case_PackageField :: IO ()
case_PackageField = do
  let goodField = "myfield12 \"abc\""
      badField = "badbad \"abc"
      okayishField = "f abc"
  parseFailure field badField
  parseSuccess field goodField
    >>= \(key, val) -> do
      key @=? "myfield12"
      val @=? (Left "abc")
  parseSuccess field okayishField
    >>= \(key, val) -> do
      key @=? "f"
      val @=? (Left "abc")

case_PackageKey :: Assertion
case_PackageKey = do
  let key = "foo@^1.3.4, bar@blafoo234, xnu@:\n"
  parseSuccess packageKeys key
    >>= \keys -> do
      keys @=? [ PackageKey "foo" "^1.3.4"
               , PackageKey "bar" "blafoo234"
               -- yes, the version can be empty …
               , PackageKey "xnu" ""]

parseSuccess :: Parser a -> Text -> IO a
parseSuccess parser string = do
  case MP.parse parser "" string of
    (Right a) -> pure a
    (Left err) -> do
      assertFailure ("parse should succeed, but: \n"
                    <> MP.parseErrorPretty err
                    <> "for input\n" <> toS string <> "\n\"")
      panic "not reached"

parseFailure :: Parser a -> Text -> IO ()
parseFailure parser string = do
  case MP.parseMaybe parser string of
    Nothing -> pure ()
    (Just a) -> assertFailure "parse should have failed"
     
tests :: TestTree
tests = $(testGroupGenerator)
