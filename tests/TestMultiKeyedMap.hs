{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, NamedFieldPuns, ViewPatterns, NoImplicitPrelude #-}
module TestMultiKeyedMap (tests) where

import Protolude
import qualified Data.List as List
import Test.Tasty (TestTree)
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import qualified Data.MultiKeyedMap as MKM

emptyMkm :: (Ord k) => MKM.MKMap k v
emptyMkm = MKM.mkMKMap (Proxy :: Proxy Int)

fromList :: [([Int], v)] -> MKM.MKMap Int v
fromList = MKM.fromList (Proxy :: Proxy Int)

prop_equality :: Property
prop_equality =
  forAll (resize 5 arbitrary :: Gen [([Int], Int)])
    $ \map1 -> forAll (resize 2 arbitrary :: Gen [([Int], Int)])
    $ \map2 ->
        -- fromList drops empty keys, the right side would be different
        keysNotEmpty map1 ==> keysNotEmpty map2 ==>
        -- equality of contents also applies to the MKM
        (map1 == map2) === (fromList map1 == fromList map2)
        -- force the contents to be the same, should always be equal
  where
    keysNotEmpty = all (\(ks, _) -> ks /= [])

-- | inserting the same value is idempotent
prop_insertIdempotent :: Int -> Int -> Property
prop_insertIdempotent key val = do
  let insVal = MKM.insert key val
  (insVal emptyMkm) === (insVal (insVal emptyMkm))

prop_fromList_emptyKeyList :: Int -> ([Int], Int) -> Property
prop_fromList_emptyKeyList val el = do
  fromList [([], val), el, ([], val)] === fromList [el]

-- | inserting the same values in a different order
-- results in the same map
prop_insertShuffled :: [(Int, Int)] -> Property
prop_insertShuffled xs =
  let xs' = List.nubBy ((==) `on` fst) xs
      insVals = foldl' (\m (k, v) -> MKM.insert k v m) emptyMkm
  in forAll (shuffle xs')
    $ \ys -> insVals xs' === insVals ys

tests :: TestTree
tests = $(testGroupGenerator)
