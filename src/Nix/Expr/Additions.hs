{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-|
Description: Additional functions that should probably be in @hnix@

Nix generation helpers. No guarantee of stability (internal!).
-}
module Nix.Expr.Additions
( stringKey, ($$=), dynamicKey
, simpleParamSet, multiParam
, (!!.)
, StrQ(..), mkStrQ, mkStrQI
) where

import Data.Fix (Fix(..))
import Data.Text (Text)
import Data.String (IsString(..))

import Nix.Expr
import Text.Regex.TDFA.Text ()
import Text.Regex.TDFA ((=~))

-- hnix helpers
-- TODO submit upstream

-- | Make a binding, but have the key be a string, not symbol.
stringKey :: Text -> NExpr -> Binding NExpr
stringKey k v = NamedVar [dynamicKey k] v
-- | Infix version of 'stringKey'.
($$=) :: Text -> NExpr -> Binding NExpr
($$=) = stringKey
infixr 2 $$=

-- | Make a dynamic key name that is only enclosed in double quotes (no antiquotes).
dynamicKey :: Text -> NKeyName NExpr
dynamicKey k = DynamicKey $ Plain $ DoubleQuoted [Plain k]

-- | shortcut to create a list of closed params, like @{ foo, bar, baz }:@
simpleParamSet :: [Text] -> Params NExpr
simpleParamSet = mkParamset . fmap (, Nothing)

-- | shortcut to create a list of multiple params, like @a: b: c:@
multiParam :: [Text] -> NExpr -> NExpr
multiParam ps expr = foldr mkFunction expr $ map Param ps

-- TODO: switch over to !. when
-- https://github.com/jwiegley/hnix/commit/8b4c137a3b125f52bb78039a9d201492032b38e8
-- goes upstream
-- | Like '!.', but automatically convert plain strings to static keys.
(!!.) :: NExpr -> Text -> NExpr
aset !!. k = Fix
  $ NSelect aset
      [(if isPlainSymbol k then StaticKey else dynamicKey) k] Nothing
  where
    -- the nix lexer regex for IDs (symbols) is 
    -- [a-zA-Z\_][a-zA-Z0-9\_\'\-]*
    isPlainSymbol :: Text -> Bool
    isPlainSymbol s = s =~ ("^[a-zA-Z_][a-zA-Z0-9_'-]*$" :: Text)
infixl 8 !!.


-- | String quotation, either a plain string (S) or antiquoted (A)
data StrQ = StrQ !Text | AntiQ !NExpr
instance IsString StrQ where
  fromString = StrQ . fromString

-- 
mkStrQtmpl :: ([Antiquoted Text NExpr] -> NString NExpr) -> [StrQ] -> NExpr
mkStrQtmpl strtr = Fix . NStr . strtr . map trans
  where trans (StrQ t) = Plain t
        trans (AntiQ r) = Antiquoted r

mkStrQ, mkStrQI :: [StrQ] -> NExpr
-- | Create a double-quoted string from a list of antiquotes/plain strings.
mkStrQ = mkStrQtmpl DoubleQuoted
-- | Create a single-quoted string from a list of antiquotes/plain strings.
mkStrQI = mkStrQtmpl Indented
