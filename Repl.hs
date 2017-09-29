{-# LANGUAGE NoImplicitPrelude, LambdaCase, RecordWildCards, OverloadedStrings #-}
module Repl where

import Prelude ()
import Distribution.Nixpkgs.Nodejs.FromYarnLock
import Protolude
import Yarn.Lock
import Yarn.Lock.Types
import Control.Concurrent.Chan

yarn = "./yl"

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
