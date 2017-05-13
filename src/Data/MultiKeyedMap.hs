{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, ScopedTypeVariables #-}
module Data.MultiKeyedMap
( MKMap
, mkMap, fromList, toList
, updateValue
, flattenKeys
) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Proxy (Proxy(..))
import qualified Data.Tuple as T
import qualified Text.Show as Show

data MKMap k v = forall ik. (Ord ik, Enum ik)
              => MKMap
                 { keyMap :: M.Map k ik
                 , highestIk :: ik
                 , valMap :: M.Map ik v }

mkMap :: forall k ik v. (Ord k, Ord ik, Enum ik, Bounded ik)
      => (Proxy ik) -> MKMap k v
mkMap _ = MKMap mempty (minBound :: ik) mempty

instance (Show k, Show v) => Show (MKMap k v) where
  showsPrec d m = Show.showString "fromList " . (showsPrec d $ toList m)

fromList :: forall ik k v. (Ord k, Ord ik, Enum ik, Bounded ik)
         => (Proxy ik) -> [([k], v)] -> MKMap k v
fromList p = L.foldl' (\m (ks, v) -> newVal ks v m) (mkMap p)

toList :: MKMap k v -> [([k], v)]
toList MKMap{keyMap, valMap} =
  map (fmap (valMap M.!) . T.swap) . M.assocs . aggregateIk $ keyMap
    where aggregateIk = M.foldlWithKey
            (\m k ik -> M.insertWith (++) ik [k] m) mempty

flattenKeys :: (Ord k) => MKMap k v -> M.Map k v
flattenKeys MKMap{keyMap, valMap} =
  M.foldlWithKey' (\m k ik -> M.insert k (valMap M.! ik) m) mempty keyMap

updateValue :: (Ord k) => k -> v -> MKMap k v -> MKMap k v
updateValue k v m@MKMap{keyMap, highestIk, valMap} =
  maybe ins upd $ M.lookup k keyMap
  where
    ins = newVal [k] v m
    upd ik = MKMap { keyMap, highestIk, valMap = M.insert ik v valMap }


-- | helper, assumes there is no such value already
--
-- Will leak space otherwise!
newVal :: (Ord k) => [k] -> v -> MKMap k v -> MKMap k v
newVal ks v MKMap{keyMap, highestIk, valMap} =
  MKMap { keyMap = L.foldl' (\m k -> M.insert k next m) keyMap ks
        , highestIk = next
        , valMap = M.insert next v valMap }
  where next = succ highestIk
