{-|
Module : Yarn.Lock.Helpers
Description : Helpers for modifying Lockfiles
Maintainer : Profpatsch
Stability : experimental

Freshly parsed 'Lockfile's are often not directly usable
e.g. because they still can contain cycles. This module
provides helpers for modifying them.
-}
module Yarn.Lock.Helpers
( decycle
) where

import qualified Data.List as L
import GHC.Stack (HasCallStack)

import qualified Data.MultiKeyedMap as MKM

import Yarn.Lock.Types
import Data.Foldable (foldl')


-- | Takes a 'Lockfile' and removes dependency cycles.
--
-- Node packages often contain those and the yarn lockfile
-- does not yet eliminate them, which may lead to infinite
-- recursions.
--
-- Invariant: Every dependency entry in each package in the
-- 'Lockfile' *must* point to an existing key, otherwise
-- the function crashes.
decycle :: HasCallStack => Lockfile -> Lockfile
decycle lf = goFold [] lf (MKM.keys lf)
  -- TODO: probably rewrite with State
  where
    -- | fold over all package keys, passing the lockfile
    goFold seen lf' pkeys =
      foldl' (\lf'' pkey -> go (pkey:seen) lf'') lf' pkeys
    -- | We get a stack of already seen packages
    -- and filter out any dependencies we already saw.
    go :: [PackageKey] -> Lockfile -> Lockfile
    go seen@(we:_) lf' =
      let ourPkg = lf' MKM.! we
          -- old deps minus the already seen ones
          -- TODO make handling of opt pkgs less of a duplication
          newDeps = dependencies ourPkg L.\\ seen
          newOptDeps = optionalDependencies ourPkg L.\\ seen
          -- we update the pkg with the cleaned dependencies
          lf'' = MKM.insert we (ourPkg { dependencies = newDeps
                               , optionalDependencies = newOptDeps }) lf'
      -- finally we do the same for all remaining deps
      in goFold seen lf'' $ newDeps ++ newOptDeps
    go [] _ = error "should not happen!"

