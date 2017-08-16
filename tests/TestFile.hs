{-# LANGUAGE OverloadedStrings, TemplateHaskell, NamedFieldPuns, ViewPatterns, NoImplicitPrelude, LambdaCase, RecordWildCards #-}
module TestFile (tests) where

import Protolude
import qualified Data.List.NonEmpty as NE
import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.HUnit
import qualified Data.Map.Strict as M

import qualified Yarn.Lock.Types as T
import qualified Yarn.Lock.File as File
import qualified Yarn.Lock.Parse as Parse

-- TODO: actually use somehow (apart from manual testing)
-- The yarn.lock file should resolve each packageKey exactly once.
--
-- No pkgname/semver combination should appear twice. That means
-- the lengths of the converted map and the list lists need to match.
-- prop_LockfileSameAmountOfKeys :: [Package] -> Bool
-- prop_LockfileSameAmountOfKeys pl = length (packageListToLockfile pl)
--                                    == length (concatMap fst pl)

emptyAst :: [(Text, Either Text Parse.PackageFields)] -> Parse.PackageFields
emptyAst = Parse.PackageFields . M.fromList

minimalAst :: [(Text, Either Text Parse.PackageFields)] -> Parse.PackageFields
minimalAst = emptyAst . ([("version", Left "0.3")] <>)

case_gitRemote :: Assertion
case_gitRemote = do
  let ref = "abcthisisaref"
      ast link_ hasUid = minimalAst $
          [ ("resolved", Left link_) ]
          <> hasUid `orEmpty` ("uid", Left ref)
  let gitRefIs parsed ref' = parsed
        <&> T.remote >>= \case
          T.GitRemote{..} -> assertEqual "url ref" ref' gitRev
          a -> assertFailure ("should be GitRemote, is " <> show a)
  astToPackageSuccess (ast ("git://github.com/bla#" <> ref) False)
    `gitRefIs` ref
  astToPackageSuccess (ast ("https://github.com/bla") True)
    `gitRefIs` ref

case_fileRemote :: Assertion
case_fileRemote = do
  let sha = "helloimref"
      good = minimalAst $
          [ ("resolved", Left $ "https://gnu.org/stallmanstoe#" <> sha) ]
      bad = minimalAst [ ("resolved", Left "https://nohash") ]
  astToPackageSuccess good
    <&> T.remote >>= \case
      T.FileRemote{..} -> assertEqual "file sha" sha fileSha1
      a -> assertFailure ("should be FileRemote, is " <> show a)
  astToPackageFailureWith (pure File.UnknownRemoteType) bad

case_missingField :: Assertion
case_missingField = do
  astToPackageFailureWith
    (File.MissingField "version"
     NE.:| [File.UnknownRemoteType]) $ emptyAst []

-- will be in protolude soon
infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

astToPackageSuccess :: Parse.PackageFields -> IO T.Package
astToPackageSuccess ast = case File.astToPackage ast of
  (Left errs) -> do
     assertFailure ("should have succeded, but:\n" <> show errs)
     panic "not reached"
  (Right pkg) -> pure pkg

astToPackageFailureWith :: (NE.NonEmpty File.ConversionError)
                        -> Parse.PackageFields -> IO ()
astToPackageFailureWith errs ast = case File.astToPackage ast of
  (Right _) -> assertFailure "should have failed"
  (Left actual) -> assertEqual "errors should be the same" errs actual

--TODO
{-
data Keys = Keys { a, b, c, y, z :: PackageKey }
keys :: Keys
keys = Keys (pk "a") (pk "b") (pk "c") (pk "y") (pk "z")
  where pk n = PackageKey n "0.1"

data LFs = LFs
  { lfNormal, lfEmpty, lfCycle, lfDecycled
  , lfComplex, lfComplexD :: Lockfile }
-- | Example lockfiles for tests.
-- These are put into scope in tests by use of @NamedFieldPuns@.
lfs :: LFs
lfs = LFs
  { lfNormal = (tlf [pkg' a [b, c], pkg' b [c], pkg' c []])
  , lfEmpty  = (tlf [])
  , lfCycle    = (tlf [pkg' a [b, c], pkg' b [a, c], pkg' c [c]])
  , lfDecycled = (tlf [pkg' a [b, c], pkg' b [   c], pkg' c [ ]])
  , lfComplex  = (tlf [pkg [a, z] [a, c], pkg [c, y] [c, a, z]])
  -- Hm, this test is implementation dependent. But the cycles get removed.
  , lfComplexD = (tlf [pkg [a, z] [    ], pkg [c, y] [      z]])
  }
  where pkg keys_ deps = (keys_, Package "0.1" (RemoteFile "" "") deps [])
        pkg' key = pkg [key]
        tlf = packageListToLockfile
        Keys{a,b,c,y,z} = keys

-- | Test for the 'decycle' method.
case_decycle :: Assertion
case_decycle = do
  -- print lfCycle
  lfDecycled @=? (decycle lfCycle)
  lfComplexD @=? (decycle lfComplex)
  where LFs{lfCycle, lfDecycled, lfComplex, lfComplexD} = lfs

type PkgMap = Map PackageKey Package

-- | A lockfile is basically a flat version of a recursive dependency structure.
-- 'Built' resembles the recursive version of said flat structure.
data Built = Built PackageKey [Built] deriving (Eq)
instance Show Built where
  show (Built k b) = show $ printBuild b
    where printBuild b' = Pr.list
            [Pr.tupled [Pr.text . toS $ name k, printBuild b']]

buildFromMap :: PkgMap -> [Built]
buildFromMap m = map go $ M.keys m
  where
    go :: PackageKey -> Built
    go pk = Built pk $ map go (dependencies $ m M.! pk)

-- | Checks if the flat lockfile builds a correct recursive structure.
case_built :: Assertion
case_built = do
  let
    LFs{lfNormal} = lfs
    Keys{a,b,c} = keys
    bl = Built
    ble p = Built p []
  buildFromMap (flattenKeys lfNormal)
    @?= [ bl a [bl b [ble c], ble c]
        , bl b [ble c]
        , ble c]



-}

tests :: TestTree
tests = $(testGroupGenerator)

