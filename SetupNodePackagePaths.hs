{-# LANGUAGE RecordWildCards, NoImplicitPrelude, LambdaCase, FlexibleContexts, NamedFieldPuns, OverloadedStrings #-}
import Protolude
import qualified Data.Text.IO as TIO
import qualified Control.Monad.Except as ExcT
import qualified Control.Exception as Exc
import qualified System.IO.Error as IOErr
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HML
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import qualified System.Posix.Files as PosixFiles
import Options.Applicative

import qualified Distribution.Nodejs.Package as NP

data Args
  = Args { argMode :: Mode
         , argTargetDir :: FilePath
         , argPackageDir :: FilePath }

data Mode = BinMode

args :: Parser Args
args = subparser
    ( command "bin" (info (modeCommands BinMode)
                    (progDesc "link package bin files")) )
  where
    modeCommands mode = Args
      <$> pure mode
      <*> strOption
            ( long "to"
           <> metavar "LINK_TARGET"
           <> help "folder to link to (absolute or relative from package folder)" )
      <*> strOption
            ( long "package"
           <> metavar "FOLDER"
           <> help "folder with the node package" )
  

main :: IO ()
main = execParser (info (args <**> helper)
                    (progDesc "Link various files from npm packages to folders"))
       >>= realMain

type ErrorLogger = ExceptT [Char] IO

realMain :: Args -> IO ()
realMain Args{..} = do
  let packageJsonPath = argPackageDir FP.</> "package.json"
  unlessM (Dir.doesDirectoryExist argTargetDir)
    $ die $ argTargetDir <> " is not a directory"
  unlessM (Dir.doesFileExist packageJsonPath)
    $ die $ packageJsonPath <> " does not exist"

  runExceptT
    (tryRead packageJsonPath >>= tryDecode packageJsonPath >>= go)
    >>= \case
      (Left err) -> die $ "ERROR: " <> err
      (Right _) -> pass

  where
    tryIOMsg :: ([Char] -> [Char]) -> IO a -> ErrorLogger a
    tryIOMsg errAnn = ExcT.withExceptT (errAnn . Exc.displayException) . tryIO

    tryRead :: FilePath -> ErrorLogger BL.ByteString
    tryRead fp = tryIOMsg exc $ BL.readFile fp
      where exc e = fp <> " cannot be read:\n" <> e
    tryDecode :: FilePath -> BL.ByteString -> ExceptT [Char] IO NP.Package
    tryDecode fp fileBs = do
      (pkg, warnings) <- NP.unLoggingPackage
                      <$> ExcT.ExceptT (pure $ first exc $ NP.decode fileBs)
      warn $ fmap NP.formatWarning warnings
      pure pkg
      where exc e = fp <> " cannot be decoded\n" <> toS e

    qte s = "\"" <> s <> "\""
    warn :: [Text] -> ErrorLogger ()
    warn ws = for_ ws $ liftIO . TIO.hPutStrLn stderr

    go :: NP.Package -> ErrorLogger ()
    go NP.Package{bin} = case argMode of
      BinMode -> traverse_ linkBin =<< case bin of
        -- files with names how they should be linked
        (NP.BinFiles bs) -> pure $ HML.toList bs
        -- a whole folder where everything should be linked
        (NP.BinFolder bf) -> do
          dirE <- liftIO $ tryJust
            (\e -> guard (IOErr.isDoesNotExistError e ||
                          IOErr.isPermissionError e))
            (Dir.listDirectory bf)
          case dirE of
            (Left _) -> do
              warn ["Binary folder " <> toS (qte bf) <> " could not be accessed."]
              pure []
            (Right dir) ->
              pure $ fmap (\f -> (toS f, bf FP.</> f)) dir

    -- | Link a binary file to @targetDir/name@.
    -- @relBinPath@ is relative from the package dir.
    linkBin :: (Text, FilePath) -> ErrorLogger ()
    linkBin (name, relBinPath) = do
      let canon fp = tryIOMsg
            (\e -> "Couldn’t canonicalize path " <> qte fp <> ": " <> e)
            (Dir.canonicalizePath fp)
      pkgDir <- canon argPackageDir
      binPath <- canon $ argPackageDir FP.</> relBinPath
      targetDir <- canon argTargetDir
      when (not $ pkgDir `isPrefixOf` binPath)
        $ throwError $ mconcat
          [ "The link to executable file "
          , qte relBinPath
          , " lies outside of the package folder!\n"
          , "That’s a security risk, aborting." ]
      tryIOMsg
        (\e -> "symlink could not be created: " <> e)
        (PosixFiles.createSymbolicLink binPath $ targetDir FP.</> toS name)
