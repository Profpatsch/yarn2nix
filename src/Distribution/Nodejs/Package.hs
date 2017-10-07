{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
{-|
Description: Parse and make sense of npm’s @package.json@ project files

They are documented on https://docs.npmjs.com/files/package.json and have a few gotchas. Luckily plain JSON, but the interpretation of certain fields is non-trivial (since they contain a lot of “sugar”).
-}
module Distribution.Nodejs.Package
( Package(..)
, Bin(..), Man(..), Dependencies
, decode
) where

import Protolude
import Control.Monad (fail)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as HML
import qualified System.FilePath as FP

import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as A

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

instance A.FromJSON Package where
  parseJSON = A.withObject "Package" $ \v -> do
    name            <- v .:  "name"
    version         <- v .:  "version"
    description     <- v .:? "description"
    homepage        <- v .:? "homepage"
    private         <- v .:? "private" .!= False
    scripts         <- v .:? "scripts" .!= mempty
    bin             <- parseBin name v
    man             <- parseMan name v
    license         <- v .:? "license"
    dependencies    <- v .:? "dependencies" .!= mempty
    devDependencies <- v .:? "devDependencies" .!= mempty
    pure Package{..}
    where

      parseBin packageName v = do
        -- check for existence of these fields
        binVal <- optional $ v .: "bin"
        dirBinVal <- optional $ v .: "directories" >>= (.: "bin")
        -- now check for all possible cases of the fields
        -- see npm documentation for more
        case (binVal, dirBinVal) of
          (Just _              , Just _) -> fail
            $ "`bin` and `directories.bin` must not exist at the same time"
          -- either "bin" is a direct path, then it’s linked to the package name
          (Just (A.String path),      _) -> pure $ BinFiles
            $ HML.singleton packageName (toS path)
          -- or it’s a map from names to paths
          (Just (A.Object bins),      _) -> BinFiles
            <$> traverse (A.withText "BinPath" (pure.toS)) bins
          (Just _              ,      _) -> fail
            $ "`bin` must be a path or a map of names to paths"
          (_                   , Just (A.String path)) -> pure $ BinFolder $ toS path
          (_                   , Just _) -> fail
            $ "`directories.bin` must be a path"
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

-- | convenience
decode :: BL.ByteString -> Either Text Package
decode = first toS . A.eitherDecode

