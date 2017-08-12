{-|
Module : Yarn.Lock.Types
Description : Types for yarn.lock files
Maintainer : Profpatsch
Stability : experimental
-}
module Yarn.Lock.Types where

import Protolude hiding (try)
import qualified Data.MultiKeyedMap as MKM

-- | Yarn lockfile.
--
-- It is a multi-keyed map (each value can be referenced by multiple keys).
-- This is achieved by using an intermediate key @ik@.
type Lockfile = MKM.MKMap PackageKey Package

-- | Proxy type for our MKMap intermediate key
lockfileIkProxy :: Proxy Int
lockfileIkProxy = Proxy

-- | Key that indexes package for a specific version.
data PackageKey = PackageKey
  { name           :: Text -- ^ package name
  , npmVersionSpec :: Text
  -- ^ String that specifies the version of a package.
  -- Sometimes a npm semver, sometimes an arbitrary string.
  } deriving (Show, Eq, Ord)

-- | Something with a list of 'PackageKey's pointing to it.
data Keyed a = Keyed [PackageKey] a
  deriving (Show, Eq, Ord)

-- | The actual npm package with dependencies and a way to download.
data Package = Package
  { version              :: Text         -- ^ resolved, specific version
  , remote               :: Remote
  , dependencies         :: [PackageKey] -- ^ list of dependencies
  , optionalDependencies :: [PackageKey] -- ^ list of optional dependencies
  } deriving (Eq, Show)

-- | Information on where to download the package.
data Remote
  = FileRemote
  { fileUrl     :: Text -- ^ URL to a remote file
  , fileSha1    :: Text -- ^ sha1 hash of the file (attached to the link)

  }
  | GitRemote
  { gitRepoUrl  :: Text -- ^ valid git remote URL
  , gitRev      :: Text -- ^ git tree-ish (commit, branch, &c.)
  } deriving (Eq, Show)
