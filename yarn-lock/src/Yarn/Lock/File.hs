{-|
Module : Yarn.Lock.File
Description : Convert AST to semantic data structures
Maintainer : Profpatsch
Stability : experimental

After parsing yarn.lock files in 'Yarn.Lock.Parse',
you want to convert the AST to something with more information
and ultimately get a 'T.Lockfile'.

@yarn.lock@ files don’t follow a structured approach
(like for example sum types), so information like e.g.
the remote type have to be inferred frome AST values.
-}
{-# LANGUAGE OverloadedStrings, ApplicativeDo, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Yarn.Lock.File
( fromPackages
, astToPackage
-- * Errors
, ConversionError(..)
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import qualified Data.Either.Validation as V

import qualified Yarn.Lock.Parse as Parse
import qualified Yarn.Lock.Types as T
import qualified Data.MultiKeyedMap as MKM
import Data.Text (Text)
import Data.Bifunctor (first)
import Control.Monad ((>=>))
import Control.Applicative ((<|>))
import Data.Either.Validation (Validation(Success, Failure))
import Data.Traversable (for)

-- | Press a list of packages into the lockfile structure.
--
-- It’s a dumb conversion, you should probably apply
-- the 'Yarn.Lock.Helpers.decycle' function afterwards.
fromPackages :: [T.Keyed T.Package] -> T.Lockfile
fromPackages = MKM.fromList T.lockfileIkProxy
             . fmap (\(T.Keyed ks p) -> (ks, p))

-- | Possible errors when converting from AST.
data ConversionError
  = MissingField Text
  -- ^ field is missing
  | WrongType { fieldName :: Text, fieldType :: Text }
  -- ^ this field has the wrong type
  | UnknownRemoteType
  -- ^ the remote (e.g. git, tar archive) could not be determined
  deriving (Show, Eq)

-- | Something that can parse the value of a field into type @a@.
data FieldParser a = FieldParser
  { parseField :: Either Text Parse.PackageFields -> Maybe a
    -- ^ the parsing function (Left is a simple field, Right a nested one)
  , parserName :: Text
    -- ^ name of this parser (for type errors)
  }

type Val = V.Validation (NE.NonEmpty ConversionError)

-- | Parse an AST 'PackageFields' to a 'T.Package', which has
-- the needed fields resolved.
astToPackage :: Parse.PackageFields
             -> Either (NE.NonEmpty ConversionError) T.Package
astToPackage = V.validationToEither . validate
  where
    validate :: Parse.PackageFields -> Val T.Package
    validate fs = do
      version              <- getField text "version" fs
      remote               <- checkRemote fs
      dependencies         <- getFieldOpt keylist "dependencies" fs
      optionalDependencies <- getFieldOpt keylist "optionalDependencies" fs
      pure $ T.Package{..}

    -- | Parse a field from a 'PackageFields'.
    getField :: FieldParser a -> Text -> Parse.PackageFields -> Val a
    getField = getFieldImpl Nothing
    -- | Parse an optional field and insert the empty monoid value
    getFieldOpt :: Monoid a => FieldParser a -> Text -> Parse.PackageFields -> Val a
    getFieldOpt = getFieldImpl (Just mempty)

    getFieldImpl :: Maybe a -> FieldParser a -> Text -> Parse.PackageFields -> Val a
    getFieldImpl mopt typeParser fieldName (Parse.PackageFields m)=
      first pure $ V.eitherToValidation $ do
        case M.lookup fieldName m of
          Nothing -> case mopt of
            Just opt -> Right opt
            Nothing  -> Left $ MissingField fieldName
          Just val ->
            case parseField typeParser val of
              Nothing -> Left
                (WrongType { fieldName, fieldType = parserName typeParser })
              Just a -> Right a

    -- | Parse a simple field to type 'Text'.
    text :: FieldParser Text
    text = FieldParser { parseField = either Just (const Nothing)
                       , parserName = "text" }

    packageKey :: FieldParser T.PackageKeyName
    packageKey = FieldParser
      { parseField = parseField text >=> T.parsePackageKeyName
      , parserName = "package key" }

    -- | Parse a field nested one level to a list of 'PackageKey's.
    keylist :: FieldParser [T.PackageKey]
    keylist = FieldParser
      { parserName = "list of package keys"
      , parseField = either (const Nothing)
             (\(Parse.PackageFields inner) ->
                  for (M.toList inner) $ \(k, v) -> do
                    name <- parseField packageKey (Left k)
                    npmVersionSpec <- parseField text v
                    pure $ T.PackageKey { name, npmVersionSpec }) }

    -- | Appling heuristics to the field contents to find the
    -- correct remote type.
    checkRemote :: Parse.PackageFields -> Val T.Remote
    checkRemote fs =
      -- any error is replaced by the generic remote error
      mToV (pure UnknownRemoteType)
        -- implementing the heuristics of searching for types;
        -- it should of course not lead to false positives
        -- see tests/TestLock.hs
        $ checkGit <|> checkFileLocal <|> checkFile
      where
        mToV :: e -> Maybe a -> V.Validation e a
        mToV err mb = case mb of
          Nothing -> Failure err
          Just a -> Success a
        vToM :: Val a -> Maybe a
        vToM = \case
          Success a -> Just a
          Failure _err -> Nothing

        -- | "https://blafoo.com/a/b#alonghash"
        --   -> ("https://blafoo.com/a/b", "alonghash")
        -- we assume the # can only occur exactly once
        findUrlHash :: Text -> (Text, Maybe Text)
        findUrlHash url = case Text.splitOn "#" url of
          [url']       -> (url', Nothing)
          [url', ""]   -> (url', Nothing)
          [url', hash] -> (url', Just hash)
          _           -> error "checkRemote: # should only appear exactly once!"

        checkGit :: Maybe T.Remote
        checkGit = do
          resolved <- vToM $ getField text "resolved" fs
          -- either in uid field or after the hash in the “resolved” URL
          (repo, gitRev) <- do
            let (repo', mayHash) = findUrlHash resolved
            hash <- vToM (getField text "uid" fs)
              <|> if any (`Text.isPrefixOf` resolved) ["git+", "git://"]
                  then mayHash else Nothing
            pure (repo', hash)
          pure $ T.GitRemote
            { T.gitRepoUrl = noPrefix "git+" repo , .. }

        -- | resolved fields that are prefixed with @"file:"@
        checkFileLocal :: Maybe T.Remote
        checkFileLocal = do
          resolved <- vToM $ getField text "resolved" fs
          let (file, mayHash) = findUrlHash resolved
          fileLocalPath <- if "file:" `Text.isPrefixOf` file
                           then Just $ noPrefix "file:" file
                           else Nothing
          case mayHash of
            Just hash -> pure (T.FileLocal fileLocalPath hash)
            Nothing   -> pure (T.FileLocalNoIntegrity fileLocalPath)

        checkFile :: Maybe T.Remote
        checkFile = do
          resolved <- vToM (getField text "resolved" fs)
          let (fileUrl, mayHash) = findUrlHash resolved
          case mayHash of
            Just hash -> pure (T.FileRemote fileUrl hash)
            Nothing   -> pure (T.FileRemoteNoIntegrity fileUrl)

        -- | ensure the prefix is removed
        noPrefix :: Text -> Text -> Text
        noPrefix pref hay = case Text.stripPrefix pref hay of
          Nothing -> hay
          Just t -> t
