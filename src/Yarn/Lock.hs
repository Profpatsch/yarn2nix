{-# LANGUAGE NoImplicitPrelude, LambdaCase, OverloadedStrings, RecordWildCards #-}
{-|
Module : Yarn.Lock
Description : High-level parser of yarn.lock files
Maintainer : Profpatsch
Stability : experimental

The <https://yarnpkg.com/ Yarn package manager> improves on npm,
because it writes @yarn.lock@ files that contain a complete
version resolution of all dependencies. This way a deterministic
deployment can be guaranteed.
-}
module Yarn.Lock
( T.Lockfile
, parseFile, parse
-- * Errors
, prettyLockfileError
, LockfileError(..), PackageErrorInfo(..)
) where

import Protolude
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as MP
import qualified Data.Either.Validation as V

import qualified Yarn.Lock.Types as T
import qualified Yarn.Lock.File as File
import qualified Yarn.Lock.Parse as Parse

-- | Everything that can go wrong when parsing a 'Lockfile'.
data LockfileError
  = ParseError Text
  -- ^ The initial parsing step failed
  | PackageErrors (NE.NonEmpty PackageErrorInfo)
  -- ^ a package could not be parsed from the AST
  deriving (Show, Eq)

-- | Information about package parsing errors.
data PackageErrorInfo = PackageErrorInfo
  { srcPos :: MP.SourcePos
  -- ^ the position of the package in the original file
  , convErrs :: NE.NonEmpty File.ConversionError
  -- ^ list of reasons for failure
  } deriving (Show, Eq)

-- | Convenience function, combining all parsing steps.
--
-- The resulting 'Lockfile' structure might not yet be optimal,
-- see 'File.fromPackages'.
parseFile :: FilePath -- ^ file to read
          -> IO (Either LockfileError T.Lockfile)
parseFile fp = readFile fp >>= pure . parse fp

-- | For when you want to provide only the file contents.
parse :: FilePath -- ^ name of the input file, used for the parser
      -> Text     -- ^ content of a @yarn.lock@
      -> Either LockfileError T.Lockfile
parse fp = astParse fp >=> toPackages >=> toLockfile

-- | Pretty print a parsing error with sane default formatting.
prettyLockfileError :: LockfileError -> Text
prettyLockfileError = \case
  (ParseError t) -> "Error while parsing the yarn.lock:\n"
    <> T.unlines (indent 2 (T.lines t))
  (PackageErrors errs) -> "Some packages could not be made sense of:\n"
    <> T.unlines (NE.toList $ indent 2 (join $ fmap errText errs))
  where
    indent :: Functor f => Int -> f Text -> f Text
    indent i = fmap (T.replicate i " " <>)
    errText (PackageErrorInfo{..}) =
      (pure $ "Package at " <> (toS $ MP.sourcePosPretty srcPos) <> ":")
      <> indent 2 (fmap convErrText convErrs)
    convErrText = \case
      (File.MissingField t) -> "Field " <> qu t <> " is missing."
      (File.WrongType{..})  -> "Field " <> qu fieldName
                               <> " should be of type " <> fieldType <> "."
      (File.UnknownRemoteType) -> "We don’t know this remote type."
    qu t = "\"" <> t <> "\""

-- helpers
astParse :: FilePath -> Text -> Either LockfileError [Parse.Package]
astParse fp = first (ParseError . toS . MP.parseErrorPretty)
                . MP.parse Parse.packageList fp

toPackages :: [Parse.Package] -> Either LockfileError [T.Keyed T.Package]
toPackages = first PackageErrors . V.validationToEither
                . traverse validatePackage

validatePackage :: Parse.Package
                -> V.Validation (NE.NonEmpty PackageErrorInfo) (T.Keyed T.Package)
validatePackage (T.Keyed keys (pos, fields)) = V.eitherToValidation
  $ bimap (pure . PackageErrorInfo pos) (T.Keyed keys)
    $ File.astToPackage fields

toLockfile :: [T.Keyed T.Package] -> Either LockfileError T.Lockfile
toLockfile = pure . File.fromPackages

