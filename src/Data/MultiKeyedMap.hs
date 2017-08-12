{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, ScopedTypeVariables #-}
{-|
Module : Data.MultiKeyedMap
Description : A multi-keyed map.
Maintainer : Profpatsch
Stability : experimental
-}
module Data.MultiKeyedMap
( MKMap
, at, (!)
, mkMKMap, fromList, toList
, insert
, flattenKeys, keys, values
) where

import qualified Data.Map.Strict as M
import Data.Monoid (All(..))
import qualified Data.List as L
import Data.Proxy (Proxy(..))
import qualified Data.Tuple as T
import qualified Text.Show as Show

-- TODO: add time behaviour of functions to docstrings

-- | A `Map`-like structure where multiple keys can point
--   to the same value, with corresponding abstracted interface.
--
-- Internally, we use two maps connected by an intermediate key.
-- The intermediate key (@ik@) can be anything implementing
-- 'Ord' (for 'Map') and 'Enum' (for 'succ').
data MKMap k v = forall ik. (Ord ik, Enum ik)
              => MKMap
                 { keyMap :: M.Map k ik
                 , highestIk :: ik
                 , valMap :: M.Map ik v }

-- TODO: is it possible without (Ord k)?
instance (Eq k, Ord k, Eq v) => Eq (MKMap k v) where
  -- TODO: not sure if that’s correct, add tests
  (==) m1@(MKMap { keyMap = km1
                 , valMap = vm1 })
       m2@(MKMap { keyMap = km2
                 , valMap = vm2 })
    = getAll $ foldMap All
        $ let ks1 = M.keys km1 in
        -- shortcut if the length of the value map is not equal
        [ length vm1 == length vm2
        -- the keys have to be equal (the lists are ascending)
        ,        ks1 == M.keys km2 ]
        -- now test whether every key leads to the same value
        -- I wonder if there is a more efficient way?
        ++ map (\k -> m1 ! k == m2 ! k) ks1
        -- we could test whether the values are equal,
        -- but if the implementation is correct they should
        -- all be reachable from the keys (TODO: add invariants)
   -- TODO: can (/=) be implemented more efficient than not.(==)?

-- | Find value at key. Partial. See 'M.!'.
at :: (Ord k) => MKMap k v -> k -> v
at MKMap{keyMap, valMap} k =  valMap M.! (keyMap M.! k)
-- | Operator alias of 'at'.
(!) :: (Ord k) => MKMap k v -> k -> v
(!) = at
{-# INLINABLE (!) #-}
{-# INLINABLE at #-}
infixl 9 !

-- | Create a 'MKMap' given a type for the internally used intermediate key.
mkMKMap :: forall k ik v. (Ord k, Ord ik, Enum ik, Bounded ik)
        => (Proxy ik) -- ^ type of intermediate key
        -> MKMap k v -- ^ new map
mkMKMap _ = MKMap mempty (minBound :: ik) mempty

instance (Show k, Show v) => Show (MKMap k v) where
  showsPrec d m = Show.showString "fromList " . (showsPrec d $ toList m)

-- | Build a map from a list of key\/value pairs.
fromList :: forall ik k v. (Ord k, Ord ik, Enum ik, Bounded ik)
         => (Proxy ik) -- ^ type of intermediate key
         -> [([k], v)] -- ^ list of @(key, value)@
         -> MKMap k v  -- ^ new map
-- TODO: it’s probably better to implement with M.fromList
fromList p = L.foldl' (\m (ks, v) -> newVal ks v m) (mkMKMap p)

-- | Convert the map to a list of key\/value pairs.
toList :: MKMap k v -> [([k], v)]
toList MKMap{keyMap, valMap} =
  map (fmap (valMap M.!) . T.swap) . M.assocs . aggregateIk $ keyMap
    where aggregateIk = M.foldlWithKey
            (\m k ik -> M.insertWith (++) ik [k] m) mempty

-- | “Unlink” keys that are pointing to the same value.
--
-- Returns a normal map.
flattenKeys :: (Ord k) => MKMap k v -> M.Map k v
flattenKeys MKMap{keyMap, valMap} =
  M.foldlWithKey' (\m k ik -> M.insert k (valMap M.! ik) m) mempty keyMap

-- | Return a list of all keys.
keys :: (Ord k) => MKMap k v -> [k]
keys = M.keys . flattenKeys

-- | Return a list of all values.
values :: MKMap k v -> [v]
values (MKMap _ _ valMap) = M.elems valMap

-- TODO: this is like normal insert, it doesn’t search if the value
-- already exists (where it might want to add the key instead).
-- Of course that would be O(n) in the naive implementation.
-- In that case the keyMap should probably be changed to a bimap.
-- also, naming
-- | Equivalent to 'M.insert', if the key doesn’t exist a new
-- singleton key is added.
insert :: (Ord k) => k -> v -> MKMap k v -> MKMap k v
insert k v m@MKMap{keyMap, highestIk, valMap} =
  maybe ins upd $ M.lookup k keyMap
  where
    ins = newVal [k] v m
    upd ik = MKMap { keyMap, highestIk, valMap = M.insert ik v valMap }


-- | Helper, assumes there is no such value already.
-- Will leak space otherwise!
--
-- Insert every key into the keyMap, increase the intermediate counter,
-- insert the value at new intermediate counter.
-- Overwrites all already existing keys!
newVal :: (Ord k) => [k] -> v -> MKMap k v -> MKMap k v
newVal ks v MKMap{keyMap, highestIk, valMap} =
  MKMap { keyMap = L.foldl' (\m k -> M.insert k next m) keyMap ks
        , highestIk = next
        , valMap = M.insert next v valMap }
  where next = succ highestIk
