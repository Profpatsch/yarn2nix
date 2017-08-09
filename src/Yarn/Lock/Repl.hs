{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Yarn.Lock.Repl
( module Yarn.Lock.Parse
, parseFile
) where

import Protolude
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Text (Parser)

import Yarn.Lock.Parse

parseFile :: (Show a) => FilePath -> Parser a -> IO a
parseFile fn p = MP.parse p fn <$> readFile fn >>= \case
  (Left e) -> panic $ toS $ MP.parseErrorPretty e
  (Right res) -> pure res

