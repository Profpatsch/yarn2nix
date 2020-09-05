{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, ViewPatterns, RecordWildCards, NoImplicitPrelude, LambdaCase, NamedFieldPuns, GeneralizedNewtypeDeriving, DeriveFunctor #-}
-- TODO: remove exts
{-|
Description: IO-based resolving of missing hashes

Resolving a 'YLT.Lockfile' and generating all necessary data (e.g. hashes), so that it can be converted to a nix expression. Might need IO & network access to succeed.
-}
module Distribution.Nixpkgs.Nodejs.ResolveLockfile
( resolveLockfileStatus
, Resolved(..), ResolvedLockfile
) where

import Protolude
import qualified Control.Monad.Trans.Except as E
import qualified Data.List.NonEmpty as NE
import qualified Data.MultiKeyedMap as MKM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT
import qualified System.Process as Process

import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Monad.STM as STM

import Distribution.Nixpkgs.Nodejs.RunConfig (RunConfig (..))

import qualified Yarn.Lock.Types as YLT

nixPrefetchGitPath :: FilePath
nixPrefetchGitPath = "nix-prefetch-git"

maxFetchers :: Int
maxFetchers = 5

-- | A thing whose hash is already known (“resolved”).
--
-- Only packages with known hashes are truly “locked”.
data Resolved a = Resolved
  { hashSum :: Text
  , resolved :: a
  } deriving (Show, Eq, Functor)

-- | In order to write a nix file, all packages need to know their shasums first.
type ResolvedLockfile = MKM.MKMap YLT.PackageKey (Resolved YLT.Package)

-- | Resolve all packages by downloading their sources if necessary.
--
--   Respects 'runOffline' from 'RunConfig': If it is 'True', it throws
--   an error as soon as it would need to download something which is the
--   case for 'YLT.GitRemote'.
resolveLockfileStatus :: RunConfig -> (Chan YLT.Remote) -> YLT.Lockfile
                      -> IO (Either (NE.NonEmpty Text) ResolvedLockfile)
resolveLockfileStatus cfg msgChan lf = Async.withTaskGroup maxFetchers $ \taskGroup -> do
  job <- STM.atomically $ Async.mapReduce taskGroup
           $ fmap (\(ks, pkg) -> (:[]) <$> (E.runExceptT $ do
                        liftIO $ writeChan msgChan (YLT.remote pkg)
                        res <- resolve pkg
                        pure (ks, res)))
               $ MKM.toList lf
  resolved <- Async.wait job
  case partitionEithers resolved of
    (x:xs, _ ) -> pure $ Left $ x NE.:| xs
    (_   , ys) -> pure $ Right $ MKM.fromList YLT.lockfileIkProxy ys

  where
    resolve :: YLT.Package -> E.ExceptT Text IO (Resolved YLT.Package)
    resolve pkg = case YLT.remote pkg of
      YLT.FileRemote{..} -> pure $ r fileSha1
      YLT.FileLocal{..}  -> pure $ r fileLocalSha1
      YLT.GitRemote{..}  -> if runOffline cfg
                              then E.throwE $ "Refusing to resolve \"git+"
                              <> gitRepoUrl <> "#" <> gitRev
                              <> "\" because --offline is set"
                              else r <$> fetchFromGit gitRepoUrl gitRev
      YLT.FileRemoteNoIntegrity{..} -> E.throwE
        $ "The remote "
        <> fileNoIntegrityUrl
        <> " does not specify a sha1 hash in the yarn.lock file, which we don’t support (yet)"
      YLT.FileLocalNoIntegrity{..} -> E.throwE
        $ "The local file "
        <> fileLocalNoIntegrityPath
        <> " does not specify a sha1 hash in the yarn.lock file, which we don’t support (yet)"
      where
        r sha = Resolved { hashSum = sha, resolved = pkg }

    fetchFromGit :: Text -> Text -> E.ExceptT Text IO Text
    fetchFromGit repo rev = do
      res <- liftIO $ Process.readProcessWithExitCode nixPrefetchGitPath
               ["--url", toS repo, "--rev", toS rev, "--hash", "sha256"] ""
      case res of
        ((ExitFailure _), _, err) -> E.throwE $ toS err
        (ExitSuccess, out, _) -> E.ExceptT . pure
          $ first (\decErr -> "parsing json output failed:\n"
                    <> toS decErr <> "\nThe output was:\n" <> toS out)
            $ do val <- Aeson.eitherDecode' (toS out)
                 AesonT.parseEither
                   (Aeson.withObject "PrefetchOutput" (Aeson..: "sha256")) val
