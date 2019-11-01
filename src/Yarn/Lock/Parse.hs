{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-|
Module : Yarn.Lock.Parse
Description : Parser for yarn.lock files
Maintainer : Profpatsch
Stability : experimental

This module provides a parser for the AST of @yarn.lock@ files.
-}
module Yarn.Lock.Parse
( PackageFields(..), Package
-- * Parsing
-- ** Re-export
, Parser
-- ** Parsers
, packageList
, packageEntry
-- * Internal Parsers
, field, nestedField, simpleField
, packageKeys
) where

import Protolude hiding (try, some, many)
import qualified Data.Char as Ch
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad (fail)

import Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- import qualified Data.MultiKeyedMap as MKM
-- import Data.Proxy (Proxy(..))

import qualified Yarn.Lock.Types as YLT


-- | We use a simple (pure) @Megaparsec@ parser.
type Parser = Parsec Void Text

-- | The @yarn.lock@ format doesn’t specifically include a fixed scheme,
-- it’s just an unnecessary custom version of a list of fields.
--
-- An field can either be a string or more fields w/ deeper indentation.
--
-- The actual conversion to semantic structures needs to be done afterwards.
newtype PackageFields = PackageFields (Map Text (Either Text PackageFields))
  deriving (Show, Eq, Semigroup, Monoid)

-- | A parsed 'Package' AST has one or more keys, a position in the original files
-- and a collection of fields.
type Package = YLT.Keyed (SourcePos, PackageFields)


-- | Parse a complete yarn.lock into an abstract syntax tree,
-- keeping the source positions of each package entry.
packageList :: Parser [Package]
packageList = MP.many $ (skipMany (comment <|> MP.string "\n")) *> packageEntry
                where
                  comment :: Parser (Tokens Text)
                  comment = MP.char '#' *> takeWhileP Nothing (/= '\n')

-- | A single Package.
--
-- Example:
--
-- @
-- handlebars@^4.0.4:
--   version "4.0.6"
--   resolved "https://registry.yarnpkg.com/handlebars/-/handlebars-4.0.6.tgz#2ce4484850537f9c97a8026d5399b935c4ed4ed7"
--   dependencies:
--     async "^1.4.0"
--     optimist "^0.6.1"
--     source-map "^0.4.4"
--   optionalDependencies:
--     uglify-js "^2.6"
--     "
-- @
packageEntry :: Parser (YLT.Keyed (SourcePos, PackageFields))
packageEntry = label "package entry" $ do
  pos <- getSourcePos
  -- A package entry is a non-indented
  (keys, pkgs) <- nonIndented
            -- block that has a header of package keys
            -- and an indented part that contains fields
            $ indentedFieldsWithHeader packageKeys
  pure $ YLT.Keyed keys (pos, pkgs)

-- | The list of PackageKeys that index the same Package
--
-- @
-- align-text@^0.1.1, align-text@^0.1.3:\\n
-- @
packageKeys :: Parser (NE.NonEmpty YLT.PackageKey)
packageKeys = label "package keys" $ do
  firstEls <- many (try $ lexeme $ packageKey ":," <* MP.char ',')
  lastEl   <-                      packageKey ":"  <* MP.char ':'
  pure $ NE.fromList $ firstEls <> [lastEl]

-- | A packageKey is @\<package-name\>\@\<semver\>@;
--
-- If the semver contains spaces, it is also quoted with @"@.
packageKey :: [Char] -> Parser YLT.PackageKey
packageKey separators = inString (pkgKey "\"")
         -- if no string delimiters is used we need to check for the separators
         -- this file format is shit :<
         <|> pkgKey separators
         <?> "package key"
  where
    pkgKey :: [Char] -> Parser YLT.PackageKey
    pkgKey valueChars = label "package key" $ do
      key <- someTextOf (MP.noneOf valueChars)
      -- okay, here’s the rub:
      -- `@` is used for separation, but package names can also
      -- start with the `@` character (so-called “scoped packages”).
      -- Furthermore, versions can contain `@` as well.
      -- This file format is a pile of elephant shit.
      case breakDrop '@' key of
        ("", rest) -> case breakDrop '@' rest of
          -- scoped key with empty name
          ("", _) -> emptyKeyErr key
          -- scoped key ("@scope/package")
          (scopedName, ver) -> YLT.PackageKey
            <$> scoped (T.cons '@' scopedName) <*> pure ver
        -- just a simple key
        (name, ver) -> pure $ YLT.PackageKey (YLT.SimplePackageKey name) ver

    emptyKeyErr :: Text -> Parser a
    emptyKeyErr key = fail
      ("packagekey: package name can not be empty (is: "
      <> toS key <> ")")

    -- | Like 'T.breakOn', but drops the separator char.
    breakDrop :: Char -> Text -> (Text, Text)
    breakDrop c str = case T.breakOn (T.singleton c) str of
      (s, "") -> (s, "")
      (s, s') -> (s, T.drop 1 s')

    -- | Parses a (scoped) package key and throws an error if misformatted.
    scoped n = maybe
      (fail $ "packageKey: scoped variable must be of form @scope/package"
           <> " (is: " <> toS n <> ")")
      pure $ YLT.parsePackageKeyName n

-- | Either a simple or a nested field.
field :: Parser (Text, Either Text PackageFields)
field = try nested <|> simple <?> "field"
  where
    simple = fmap Left <$> simpleField
    nested = fmap Right <$> nestedField

-- | A key-value pair, separated by space.
-- Key any value may be enclosed in "".
-- Returns key and value.
simpleField :: Parser (Text, Text)
simpleField = (,) <$> lexeme (strSymbolChars <|> symbolChars)
                  -- valueChars may be in Strings or maybe not >:
                  -- this file format is absolute garbage
                  <*> (strValueChars <|> valueChars)
                  <?> "simple field"
  where
    valueChars, strValueChars :: Parser Text
    valueChars = someTextOf (MP.noneOf ("\n\r\"" :: [Char]))
    strSymbolChars = inString $ symbolChars
    strValueChars = inString $ valueChars
      -- as with packageKey semvers, this can be empty
      <|> (pure T.empty <?> "an empty value field")

-- | Similar to a @simpleField@, but instead of a string
-- we get another block with deeper indentation.
nestedField :: Parser (Text, PackageFields)
nestedField = label "nested field" $
  indentedFieldsWithHeader (symbolChars <* MP.char ':')


-- internal parsers

-- | There are two kinds of indented blocks:
-- One where the header is the package
-- and one where the header is already a package field key.
indentedFieldsWithHeader :: Parser a -> Parser (a, PackageFields)
indentedFieldsWithHeader header = indentBlock $ do
    -- … block that has a header of package keys
    hdr <- header
    -- … and an indented part that contains fields
    pure $ MPL.IndentSome Nothing
      (\fields -> pure (hdr, toPfs fields)) field
  where
    toPfs :: [(Text, Either Text PackageFields)] -> PackageFields
    toPfs = PackageFields . M.fromList

-- | Characters allowed in key symbols.
-- 
-- TODO: those are partly npm package names, so check the allowed symbols, too.
--
-- Update: npm doesn’t specify the package name format, at all.
-- Apart from the length.
-- Update: According to https://docs.npmjs.com/misc/scope
-- the package name format is “URL-safe characters, no leading dots or underscores” TODO
symbolChars :: Parser Text
symbolChars = label "key symbol" $ someTextOf $ MP.satisfy
  (\c -> Ch.isAscii c &&
     (Ch.isLower c || Ch.isUpper c || Ch.isNumber c || c `elem` special))
  where special = "-_.@/" :: [Char]


-- text versions of parsers & helpers

someTextOf :: Parser Char -> Parser Text
someTextOf c = T.pack <$> some c

-- | parse everything as inside a string
inString :: Parser a -> Parser a
inString = between (MP.char '"') (MP.char '"')

-- lexers

-- | Parse whitespace.
space :: Parser ()
space = MPL.space (void MP.spaceChar)
                  (MPL.skipLineComment "# ")
                  (void $ MP.satisfy (const False))

-- | Parse a lexeme.
lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme space

-- | Ensure parser is not indented.
nonIndented :: Parser a -> Parser a
nonIndented = MPL.nonIndented space
indentBlock :: Parser (MPL.IndentOpt Parser a b)
            -> Parser a
indentBlock = MPL.indentBlock space
