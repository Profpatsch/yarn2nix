{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
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

-- | bin files from name to file
data Bin = BinFiles (HML.HashMap Text FilePath) | BinFolder FilePath deriving (Show, Eq)
-- | man files from name to file
data Man = ManFiles (HML.HashMap Text FilePath) deriving (Show, Eq)
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
    -- TODO: the bin and man parsers are way too lenient
    -- they should fail if the types are wrong
    where parseBin name v = do
            let getBin f = BinFiles . f <$> v .: "bin"
            bin <- optional (getBin (HML.singleton name)
                         <|> getBin identity)
            dirBin <- optional (BinFolder <$> (do dirs <- v .: "directories"
                                                  dirs .: "bin"))
            case (bin, dirBin) of
              (Just _, Just _) -> fail
                "`bin` and `directories.bin` must not exist at the same time"
              (Just files, _) -> pure files
              (_, Just dirs)  -> pure dirs
              (Nothing, Nothing) -> pure . BinFiles $ mempty
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

