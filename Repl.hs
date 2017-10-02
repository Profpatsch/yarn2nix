{-# LANGUAGE NoImplicitPrelude, LambdaCase, RecordWildCards, OverloadedStrings #-}
module Repl where

import Prelude ()
import Distribution.Nixpkgs.Nodejs.ResolveLockfile
import Distribution.Nixpkgs.Nodejs.OptimizedNixOutput
import Protolude
import Yarn.Lock
import Yarn.Lock.Types
import Control.Concurrent.Chan
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import Nix.Pretty (prettyNix)

yarn = "./yl"

ps = do
  Right res <- makeitso
  putDoc $ prettyNix $ mkPackageSet $ convertLockfile res

makeitso = do
  Right f <- parseFile yarn
  ch <- newChan
  thrd <- forkIO $ forever $ do
    readChan ch >>= \case
      FileRemote{..} -> pass
      GitRemote{..} -> print $ "Downloading " <> gitRepoUrl
  lf <- resolveLockfileStatus ch f
  killThread thrd
  pure lf
