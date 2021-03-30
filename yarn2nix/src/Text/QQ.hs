{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Data.Text as Text
import Data.String (lines, unlines)

-- | A quasiquoter for text. Will strip the leading whitespace of the first line
-- and any leading empty line.
-- See @fromIndented@ for stripping logic.
text :: QuasiQuoter
text = QuasiQuoter
  { quotePat = error "Quasiquoter used in pattern context."
  , quoteType = error "Quasiquoter used in type context."
  , quoteDec = error "Quasiquoter used in declaration context."
  , quoteExp = appE [| Text.pack . fromIndented |] . stringE
  }

-- | If the first line is empty, strip it;
-- the first line’s indentation is used as the indentation for the whole thing
--
-- It is not checked whether each following line has the same amount of leading spaces. The indent length will be dropped regardless.
fromIndented :: String -> String
fromIndented s = unlines $
  case lines s of
    [] -> [""]
    (first:rest) -> case first of
      "" -> case rest of
        [] -> [""]
        (second:_) -> dedent second rest
      _ -> dedent first (first:rest)
  where
    dedent first allLines =
      let indent = length $ takeWhile (== ' ') $ first
      in map (drop indent) allLines
