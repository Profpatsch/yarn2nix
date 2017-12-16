{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, RecordWildCards, LambdaCase #-}
{-|
Description: Parse and make sense of npm’s @package.json@ project files

They are documented on https://docs.npmjs.com/files/package.json and have a few gotchas. Luckily plain JSON, but the interpretation of certain fields is non-trivial (since they contain a lot of “sugar”).
-}
module Distribution.Nodejs.Package
( -- * Parsing @package.json@
  LoggingPackage(..), decode
, Warning(..), formatWarning
  -- * @package.json@ data
, Package(..)
, Bin(..), Man(..), Dependencies
) where

import Protolude
import Control.Monad (fail)
import qualified Control.Monad.Writer.Lazy as WL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import qualified System.FilePath as FP

import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

-- | npm `package.json`. Not complete.
--
-- See https://docs.npmjs.com/files/package.json
data Package = Package
  { name :: Text
  , version :: Text
  , description :: Maybe Text
  , homepage :: Maybe Text
  , private :: Bool
  , scripts :: HML.HashMap Text Text
  , bin :: Bin
  , man :: Man
  , license :: Maybe Text
  , dependencies :: Dependencies
  , devDependencies :: Dependencies
  } deriving (Show, Eq)

-- | 'Package' with a potential bunch of parsing warnings.
-- Note the 'A.FromJson' instance.
newtype LoggingPackage = LoggingPackage
  { unLoggingPackage :: (Package, [Warning]) }

-- | Possible warnings from parsing.
data Warning
  = WrongType
  { wrongTypeField :: Text
  , wrongTypeDefault :: Text }
  | PlainWarning Text

-- | The package’s executable files.
data Bin
  = BinFiles (HML.HashMap Text FilePath)
  -- ^ map of files from name to their file path (relative to package path)
  | BinFolder FilePath
  -- ^ a folder containing all executable files of the project (also relative)
  deriving (Show, Eq)

-- | The package’s manual files.
data Man
  = ManFiles (HML.HashMap Text FilePath)
  -- ^ map of files from name to their file path (relative to package path)
  deriving (Show, Eq)

-- | Dependencies of a package.
type Dependencies = HML.HashMap Text Text

-- | See https://github.com/npm/normalize-package-data for
-- normalization steps used by npm itself.
instance A.FromJSON LoggingPackage where
  parseJSON = A.withObject "Package" $ \v -> fmap LoggingPackage . WL.runWriterT $ do
    let
      l :: AT.Parser a -> WL.WriterT [Warning] AT.Parser a
      l = WL.WriterT . fmap (\a -> (a, []))
      tryWarn :: (AT.FromJSON a, Show a)
              => Text -> a -> WL.WriterT [Warning] AT.Parser a
      tryWarn field def = lift (v .:? field .!= def)
                          <|> WL.writer (def, [WrongType field (show def)])
    name            <- l $ v .:  "name"
    version         <- l $ v .:  "version"
    description     <- tryWarn "description" Nothing
    homepage        <- tryWarn "homepage" Nothing
    private         <- tryWarn "private" False
    scripts         <- l $ v .:? "scripts" .!= mempty
    bin             <- parseBin name v
    man             <- l $ parseMan name v
    license         <- tryWarn "license" Nothing
    dependencies    <- l $ v .:? "dependencies" .!= mempty
    devDependencies <- l $ v .:? "devDependencies" .!= mempty
    pure Package{..}
    where

      parseBin :: Text -> AT.Object -> WL.WriterT [Warning] AT.Parser Bin
      parseBin packageName v = do
        -- check for existence of these fields
        binVal <- lift $ optional $ v .: "bin"
        dirBinVal <- lift $ optional $ v .: "directories" >>= (.: "bin")
        -- now check for all possible cases of the fields
        -- see npm documentation for more
        case (binVal, dirBinVal) of
          (Just _              , Just _) ->
            WL.writer (BinFiles mempty, [PlainWarning
              "`bin` and `directories.bin` must not exist at the same time."])
          -- either "bin" is a direct path, then it’s linked to the package name
          (Just (A.String path),      _) -> pure $ BinFiles
            $ HML.singleton packageName (toS path)
          -- or it’s a map from names to paths
          (Just (A.Object bins),      _) -> lift $ BinFiles
            <$> traverse (A.withText "BinPath" (pure.toS)) bins
          (Just _              ,      _) -> fail
            $ "`bin` must be a path or a map of names to paths."
          (_                   , Just (A.String path)) -> pure $ BinFolder $ toS path
          (_                   , Just _) -> fail
            $ "`directories.bin` must be a path."
          -- if no executables are given, return an empty set
          (Nothing             , Nothing) -> pure . BinFiles $ mempty

      -- TODO: parsing should be as thorough as with "bin"
      parseMan name v = do
        let getMan f = ManFiles . f <$> v .: "man"
            extractName :: FilePath -> (Text, FilePath)
            extractName file =
              let f = T.pack $ FP.takeFileName file
              in if name `T.isPrefixOf` f
                  then (name, file)
                  else (name <> "-" <> f, file)
        -- TODO: handle directories.man
        (getMan (HML.fromList . map extractName)
            <|> getMan (HML.fromList . (:[]) . extractName)
            <|> pure (ManFiles mempty))

-- | Convenience decoding function.
decode :: BL.ByteString -> Either Text LoggingPackage
decode = first toS . A.eitherDecode

-- | Convert a @package.json@ parsing warning to plain text.
formatWarning :: Warning -> Text
formatWarning = ("Warning: " <>) . \case
  (WrongType field def) ->
    "Field \"" <> field <> "\" has the wrong type. Defaulting to " <> def <> "."
  (PlainWarning t) -> t
