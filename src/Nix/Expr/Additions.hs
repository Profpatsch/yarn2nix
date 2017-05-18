{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Nix.Expr.Additions where

import Data.Fix (Fix(..))
import Data.Text (Text)

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

-- TODO: switch over to $= when
-- https://github.com/jwiegley/hnix/commit/8b4c137a3b125f52bb78039a9d201492032b38e8
-- goes upstream
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
