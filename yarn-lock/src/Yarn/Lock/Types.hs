{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}
{-|
Module : Yarn.Lock.Types
Description : Types for yarn.lock files
Maintainer : Profpatsch
Stability : experimental
-}
module Yarn.Lock.Types where

import qualified Data.MultiKeyedMap as MKM
import qualified Data.List.NonEmpty as NE
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Yarn lockfile.
--
-- It is a multi-keyed map (each value can be referenced by multiple keys).
-- This is achieved by using an intermediate key @ik@.
--
-- Attention: Might be changed to a newtype in a future release.
type Lockfile = MKM.MKMap PackageKey Package
-- TODO newtype Lockfile = Lockfile (MKM.MKMap PackageKey Package)

-- | Proxy type for our MKMap intermediate key
lockfileIkProxy :: Proxy Int
lockfileIkProxy = Proxy

-- | Key that indexes package for a specific version.
data PackageKey = PackageKey
  { name           :: PackageKeyName -- ^ package name
  , npmVersionSpec :: Text
  -- ^ tring that specifies the version of a package;
  -- sometimes a npm semver, sometimes an arbitrary string
  } deriving (Show, Eq, Ord)

-- | The name of a package. They can be scoped, see
-- | <https://docs.npmjs.com/misc/scope> for an explanation.
data PackageKeyName
  = SimplePackageKey Text
  -- ^ just a package name
  | ScopedPackageKey Text Text
  -- ^ a scope and a package name (e.g. @types/foobar)
  deriving (Show, Eq, Ord)

-- | Try to parse a string into a package key name (scoped or not).
parsePackageKeyName :: Text -> Maybe PackageKeyName
parsePackageKeyName n = case Text.stripPrefix "@" n of
  Nothing -> Just $ SimplePackageKey n
  Just sc -> case Text.breakOn "/" sc of
    (_, "") -> Nothing
    (scope, pkg) -> Just $ ScopedPackageKey scope (Text.drop 1 pkg)

-- | Something with a list of 'PackageKey's pointing to it.
data Keyed a = Keyed (NE.NonEmpty PackageKey) a
  deriving (Show, Eq, Ord, Functor)

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
  | FileRemoteNoIntegrity
  { fileNoIntegrityUrl :: Text -- ^ URL to a remote file
  }
  | GitRemote
  { gitRepoUrl  :: Text -- ^ valid git remote URL
  , gitRev      :: Text -- ^ git tree-ish (commit, branch, &c.)
  }
  -- this is a bit of an oddidity, but what isn’t
  | FileLocal
  { fileLocalPath :: Text -- ^ (relative) path to file on the local machine
  , fileLocalSha1 :: Text -- ^ sha1 hash of the file (attached to the link)
  }
  | FileLocalNoIntegrity
  { fileLocalNoIntegrityPath :: Text -- ^ (relative) path to file on the local machine
  } deriving (Eq, Show)
