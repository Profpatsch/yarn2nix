{-# LANGUAGE OverloadedStrings, TemplateHaskell, NamedFieldPuns, ViewPatterns, NoImplicitPrelude #-}
module TestLock (tests) where

import Protolude
import Yarn.Lock
import Data.MultiKeyedMap hiding (keys)
import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.HUnit
import qualified Text.Show as S
import qualified Text.PrettyPrint.ANSI.Leijen as Pr
import qualified Data.Map.Strict as M

data Keys = Keys { a, b, c, y, z :: PackageKey }
keys :: Keys
keys = Keys (pk "a") (pk "b") (pk "c") (pk "y") (pk "z")
  where pk n = PackageKey n "0.1"

data LFs = LFs
  { lfNormal, lfEmpty, lfCycle, lfDecycled
  , lfComplex, lfComplexD :: Lockfile }
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

case_decycle :: Assertion
case_decycle = do
  -- print lfCycle
  lfDecycled @=? (decycle lfCycle)
  lfComplexD @=? (decycle lfComplex)
  where LFs{lfCycle, lfDecycled, lfComplex, lfComplexD} = lfs
  

type PkgMap = Map PackageKey Package

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

-- lfBuilt :: Lockfile -> [Built]
-- lfBuilt = buildFromMap . flattenKeys

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


tests :: TestTree
tests = $(testGroupGenerator)
