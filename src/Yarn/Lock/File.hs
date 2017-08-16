{-|
Module : Yarn.Lock.File
Description : Semantic info about yarn.lock files
Maintainer : Profpatsch
Stability : experimental

After parsing yarn.lock files in 'Yarn.Lock.Parse',
you want to find out semantic information from the AST
and ultimately get a 'Lockfile'.
-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo, RecordWildCards, NamedFieldPuns #-}
module Yarn.Lock.File
( fromPackages
, astToPackage, ConversionError(..)
) where

import Protolude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import qualified Data.Either.Validation as V

import qualified Yarn.Lock.Parse as Parse
import qualified Yarn.Lock.Types as T
import qualified Yarn.Lock.Helpers as Helpers
import qualified Data.MultiKeyedMap as MKM

-- | Press a list of packages into the lockfile structure.
--
-- It’s a dumb conversion, you should probably apply
-- the 'Helpers.decycle' function afterwards.
fromPackages :: [T.Keyed T.Package] -> T.Lockfile
fromPackages = MKM.fromList T.lockfileIkProxy
             . fmap (\(T.Keyed ks p) -> (ks, p))

data ConversionError
  = MissingField Text
  | WrongType { fieldName :: Text, fieldType :: Text }
  | UnknownRemoteType
  deriving (Show, Eq)

data FieldParser a = FieldParser
  { parseField :: Either Text Parse.PackageFields -> Maybe a
  , parserName :: Text }
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
          Just val -> note
            (WrongType { fieldName, fieldType = parserName typeParser })
            $ parseField typeParser val

    -- | Parse a simple field to type 'Text'.
    text :: FieldParser Text
    text = FieldParser { parseField = either Just (const Nothing)
                       , parserName = "text" }

    -- | Parse a field nested one level to a list of 'PackageKey's.
    keylist :: FieldParser [T.PackageKey]
    keylist = FieldParser
      { parserName = "list of package keys"
      , parseField = either (const Nothing)
             (\(Parse.PackageFields inner) ->
                  for (M.toList inner) $ \(k, v) -> do
                    npmVersionSpec <- parseField text v
                    pure $ T.PackageKey { T.name = k, ..}) }

    -- | Appling heuristics to the field contents to find the
    -- correct remote type.
    checkRemote :: Parse.PackageFields -> Val T.Remote
    checkRemote fs =
      -- any error is replaced by the generic remote error
      mToV (pure UnknownRemoteType)
        -- implementing the heuristics of searching for types;
        -- it should of course not lead to false positives
        -- see tests/TestLock.hs
        $ checkGit <|> checkFile
      where
        mToV :: e -> Maybe a -> V.Validation e a
        mToV err = V.eitherToValidation . note err
        vToM :: Val a -> Maybe a
        vToM = hush . V.validationToEither

        -- | "https://blafoo.com/a/b#alonghash" -> "alonghash"
        findUrlHash :: Text -> Maybe Text
        findUrlHash url = pure (Text.splitOn "#" url)
                          -- or the whole link is used
                          >>= guarded (\xs -> length xs > 1)
                          >>= lastMay
                          -- should also not end with #
                          >>= guarded (/= "")

        checkGit = do
          resolved <- vToM $ getField text "resolved" fs
          -- either in uid field or after the hash in the “resolved” URL
          gitRev <- vToM (getField text "uid" fs)
            <|> if any (`Text.isPrefixOf` resolved) ["git+", "git://"]
                then findUrlHash resolved else Nothing
          pure $ T.GitRemote { T.gitRepoUrl = resolved, .. }

        checkFile = do
          fileUrl <- vToM (getField text "resolved" fs)
          fileSha1 <- findUrlHash fileUrl
          pure $ T.FileRemote{..}
