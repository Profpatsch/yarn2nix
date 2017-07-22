{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
{-|
Module : Yarn.Lock.Parse
Description : Parser for yarn.lock files
Maintainer : Profpatsch
Stability : experimental

This module provides a parser for @yarn.lock@ files.
-}
module Yarn.Lock
( PackageEntry, PackageList
, Yarn.Lock.parse
-- | = Parsers
, lockfile
, packageListToLockfile, packageList
, packageEntry, packageKeys, packageKey, package
) where

import Protolude hiding (try)
import qualified Data.List as L
import Data.String (String)
import Text.Megaparsec as MP
import Text.Megaparsec.Text
import qualified Data.Text as T

import qualified Data.MultiKeyedMap as MKM
import Data.Proxy (Proxy(..))

import Yarn.Lock.Types


-- | A entry as it appears in the yarn.lock representation.
type PackageEntry = ([PackageKey], Package)
-- | Convenience alias.
type PackageList = [PackageEntry]

-- | Convenience function that converts errors to Text.
--
-- The actual parsers are below.
parse :: Text -- ^ name of source file
      -> Text -- ^ input for parser
      -> Either Text Lockfile
parse src inp = first (T.pack . parseErrorPretty)
              $ MP.parse lockfile (T.unpack src) inp

-- TODO: actually use somehow (apart from manual testing)
-- The yarn.lock file should resolve each packageKey exactly once.
--
-- No pkgname/semver combination should appear twice. That means
-- the lengths of the converted map and the list lists need to match.
-- prop_LockfileSameAmountOfKeys :: PackageList -> Bool
-- prop_LockfileSameAmountOfKeys pl = length (packageListToLockfile pl)
--                                    == length (concatMap fst pl)


-- HALP, I don’t know how to parser.
-- It appears to be a more general format which somewhat resembles yaml.
-- The code below conflates the format & the semantics of yarn.lock files.
-- It should be separated sometime, to make parsing easier.

-- | Convenience function that applies @packageListToLockfile@.
lockfile :: Parser Lockfile
lockfile = packageListToLockfile <$> packageList

-- | The yarn.lock file is basically a hashmap with multi-keyed entries.
--
-- This should press it into our Lockfile Map.
packageListToLockfile :: PackageList -> Lockfile
packageListToLockfile = MKM.fromList lockfileIkProxy

  -- foldl' go mempty
  -- where go lf (keys, pkg) = foldl' (\lf' key' -> M.insert key' pkg lf') lf keys

-- | Parse a complete yarn.lock into exactly the same representation.
--
-- You can apply @packageListToLockfile@ to make it usable.
packageList :: Parser PackageList
packageList = many $ (skipMany (comment <|> eol)) *> packageEntry
                where comment = char '#' *> manyTill anyChar eol

-- | A single PackageEntry.
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
-- @
packageEntry :: Parser PackageEntry
packageEntry = (,) <$> packageKeys <*> package <?> "package entry"

-- | The list of PackageKeys that index the same Package
--
-- @
-- align-text@^0.1.1, align-text@^0.1.3:\\n
-- @
packageKeys :: Parser [PackageKey]
packageKeys = sepBy1 packageKey (string ", ") <* (char ':') <* eol <?> "package keys"

-- | A packageKey is @\<package-name\>\@\<semver\>@;
--
-- If the semver contains spaces, it is also quoted with @"@.
packageKey :: Parser PackageKey
packageKey = label "package key" $ inString pkgKey <|> pkgKey
  where
    pkgKey = PackageKey
      -- everything until the version, sep is @
      <$> (someTextUntilSep '@' <?> "package name part of package key")
      -- a version is anything but the , (used for seperating package keys)
      -- or : (used to close the packageKeys line)
      -- could be more specific (version is semver), but I’m lazy
      <*> (someText (noneOf "\",:") <?> "semver part of package key")

-- | Parses the content fields of a package.
package :: Parser Package
package = Package
-- TODO: order shouldn’t matter, horrible indentation scheme
  <$> (indent 2 $ key "version" stringText)
  <*> (indent 2 $ key "resolved" remoteFile)
  <*> (maybe [] identity <$> optional (indent 2 $ dependencyEntries "dependencies"))
  <*> (maybe [] identity <$> optional (indent 2 $ dependencyEntries "optionalDependencies"))


-- internal parsers

-- | the “resolved”-field contains the link and the hash
remoteFile :: Parser Remote
remoteFile = label "file link with hash" $ RemoteFile
  <$> someTextUntilSep '#'
  <*> stringText

-- | dependency field of a package
dependencyEntries :: String -> Parser [PackageKey]
dependencyEntries key' = label (key' <>" field") $ do
  _ <- string (key' <>":") <* eol
  -- TODO: cool indentation handling
  some (indent 4 dep)
  where
    -- It’s a bit like a key below, but the value of the key is not known.
    -- Here’s where the format should get its own AST, but I’m too lazy right now.
    dep = PackageKey <$> someTextUntilSep ' ' <*> inString stringText <* eol <?> "a dependency entry"

-- | A key-value pair, separated by space. The value is enclosed in "".
--
-- The given parser is used to parse the value and should not parse ".
key :: String -> Parser a -> Parser a
key name' val = label ("key " <> name') $
  string name' *> char ' ' *> inString val  <* eol


-- text versions of parsers & helpers

someText :: Parser Char -> Parser Text
someText c = T.pack <$> some c

-- | parse everything as inside a string
-- TODO: this breaks the 'between' abstraction, can it be avoided somehow?
inString :: Parser a -> Parser a
inString = between (char '"') (char '"')

-- | function to annotate text inside strings (which should never parse ")
--   symptom of the broken 'between' abstraction
stringText :: Parser Text
stringText = someText (noneOf "\"") <?> "non-empty text without \""

-- | parse some text until seperator is reached
someTextUntilSep :: Char -> Parser Text
someTextUntilSep sep = T.pack <$> someTill anyChar (char sep)

-- | intend by @i@ spaces
indent :: Int -> Parser a -> Parser a
indent i p = try $ count i (char ' ') *> p
