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

import Protolude hiding (packageName)
import Control.Monad (fail)
import qualified Control.Monad.Writer.Lazy as WL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import qualified System.FilePath as FP

import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Vector as V

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
  { wrongTypeField :: Text -- ^ the field which has a wrong type
  , wrongTypeDefault :: Maybe Text -- ^ the default value, if used
  }
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

type Warn = WL.WriterT [Warning] AT.Parser
putWarning :: a -> Warning -> Warn a
putWarning a w = WL.writer (a, [w])

-- | See https://github.com/npm/normalize-package-data for
-- normalization steps used by npm itself.
instance A.FromJSON LoggingPackage where
  parseJSON = A.withObject "Package" $ \v -> fmap LoggingPackage . WL.runWriterT $ do
    let
      l :: AT.Parser a -> Warn a
      l = WL.WriterT . fmap (\a -> (a, []))
      tryWarn :: (AT.FromJSON a, Show a)
              => Text -> a -> Warn a
      tryWarn field def =
        lift (v .:? field .!= def)
        <|> putWarning def (WrongType { wrongTypeField = field
                                       , wrongTypeDefault = Just (show def) })
    name            <- l $ v .:  "name"
    version         <- l $ v .:  "version"
    description     <- tryWarn "description" Nothing
    homepage        <- tryWarn "homepage" Nothing
    private         <- tryWarn "private" False
    scripts         <- (parseMapText "scripts" =<< (tryWarn "scripts" mempty))
    bin             <- parseBin name v
    man             <- l $ parseMan name v
    license         <- tryWarn "license" Nothing
    dependencies    <- parseDependencies v "dependencies"
    devDependencies <- parseDependencies v "devDependencies"
    pure Package{..}
    where

      parseDependencies :: AT.Object -> Text -> Warn Dependencies
      parseDependencies v field =
        let def = mempty
        in  lift (v .:? field .!= def) <|>
           (lift (v .:  field) >>= \val ->
              case val of
                -- non empty arrays cause an error
                AT.Array a ->
                  if V.null a
                    then putWarning def
                      (WrongType { wrongTypeField   = field
                                 , wrongTypeDefault = Just (show def) })
                    else fail $ "\"" ++ T.unpack field
                      ++ "\" is a non empty array instead of a JSON object"
                -- if we get an object here, it's malformed
                AT.Object _ -> fail
                  $ "Could not parse object in \"" ++ T.unpack field ++ "\""
                -- everything else defaults to mempty and generates a warning
                _ -> putWarning def
                       (WrongType { wrongTypeField   = field
                                  , wrongTypeDefault = Just (show def) }))

      parseMapText :: Text -> HML.HashMap Text AT.Value
                   -> Warn (HML.HashMap Text Text)
      parseMapText fieldPath val =
        HML.mapMaybe identity <$> HML.traverseWithKey tryParse val
        where
          tryParse :: Text -> A.Value -> Warn (Maybe Text)
          tryParse key el = lift (Just <$> AT.parseJSON el)
            <|> putWarning Nothing
                  (WrongType { wrongTypeField = fieldPath <> "." <> key
                             , wrongTypeDefault = Nothing })
      parseBin :: Text -> AT.Object -> Warn Bin
      parseBin packageName v = do
        -- check for existence of these fields
        binVal <- lift $ optional $ v .: "bin"
        dirBinVal <- lift $ optional $ v .: "directories" >>= (.: "bin")
        -- now check for all possible cases of the fields
        -- see npm documentation for more
        case (binVal, dirBinVal) of
          (Just _              , Just _) ->
            putWarning (BinFiles mempty) $ PlainWarning
              "`bin` and `directories.bin` must not exist at the same time, skipping."
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
formatWarning = \case
  WrongType{..} ->
       "Field \""
    <> wrongTypeField
    <> "\" has the wrong type. "
    <> (case wrongTypeDefault of
         Just def -> "Defaulting to " <> def
         Nothing  -> "Leaving it out")
    <> "."
  (PlainWarning t) -> t
