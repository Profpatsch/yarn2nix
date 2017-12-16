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
import qualified Control.Monad.Trans.Either as E
import qualified Data.List.NonEmpty as NE
import qualified Data.MultiKeyedMap as MKM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonT
import qualified System.Process as Process

import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Monad.STM as STM

import qualified Yarn.Lock.Types as YLT

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
resolveLockfileStatus :: (Chan YLT.Remote) -> YLT.Lockfile
                      -> IO (Either (NE.NonEmpty Text) ResolvedLockfile)
resolveLockfileStatus msgChan lf = Async.withTaskGroup maxFetchers $ \taskGroup -> do
  job <- STM.atomically $ Async.mapReduce taskGroup
           $ fmap (\(ks, pkg) -> (:[]) <$> (E.runEitherT $ do
                        liftIO $ writeChan msgChan (YLT.remote pkg)
                        res <- resolve pkg
                        pure (ks, res)))
               $ MKM.toList lf
  resolved <- Async.wait job
  case partitionEithers resolved of
    (x:xs, _ ) -> pure $ Left $ x NE.:| xs
    (_   , ys) -> pure $ Right $ MKM.fromList YLT.lockfileIkProxy ys

  where
    resolve :: YLT.Package -> E.EitherT Text IO (Resolved YLT.Package)
    resolve pkg = case YLT.remote pkg of
      YLT.FileRemote{..} -> pure $ r fileSha1
      YLT.GitRemote{..}  -> r <$> fetchFromGit gitRepoUrl gitRev
      where
        r sha = Resolved { hashSum = sha, resolved = pkg }

    fetchFromGit :: Text -> Text -> E.EitherT Text IO Text
    fetchFromGit repo rev = do
      res <- liftIO $ Process.readProcessWithExitCode "nix-prefetch-git"
               ["--url", toS repo, "--rev", toS rev, "--hash", "sha256"] ""
      case res of
        ((ExitFailure _), _, err) -> E.left $ toS err
        (ExitSuccess, out, _) -> E.hoistEither
          $ first (\decErr -> "parsing json output failed:\n"
                    <> toS decErr <> "\nThe output was:\n" <> toS out)
            $ do val <- Aeson.eitherDecode' (toS out)
                 AesonT.parseEither
                   (Aeson.withObject "PrefetchOutput" (Aeson..: "sha256")) val
