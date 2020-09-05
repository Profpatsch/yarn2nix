{-# LANGUAGE NoImplicitPrelude #-}
{-|
Description: Invocation Configuration
-}
module Distribution.Nixpkgs.Nodejs.RunConfig
  ( RunConfig (..)
  , RunMode (..)
  ) where

import Protolude

-- | Type of action @yarn2nix@ is performing.
data RunMode
  = YarnLock     -- ^ Output a nix expression for a @yarn.lock@
  | NodeTemplate -- ^ Output a nix template corresponding to a @package.json@
  deriving (Show, Eq)

-- | Runtime configuration of @yarn2nix@. Typically this is determined from
--   its command line arguments and valid for the current invocation only.
data RunConfig
  = RunConfig
  { runMode         :: RunMode
  , runOffline      :: Bool            -- ^ If @True@, @yarn2nix@ will fail if it
                                       --   requires network access. Currently this means
                                       --   'Distribution.Nixpkgs.Nodejs.ResolveLockfile.resolveLockfileStatus'
                                       --   will throw an error in case resolving a hash
                                       --   requires network access.
  , runLicensesJson :: Maybe FilePath  -- ^ Optional Path to a licenses.json file
                                       --   equivalent to the lib.licenses set from
                                       --   @nixpkgs@.
  , runInputFile    :: Maybe FilePath  -- ^ File to process. If missing the appropriate
                                       --   file for the current mode from the current
                                       --   working directory is used.
  } deriving (Show, Eq)
