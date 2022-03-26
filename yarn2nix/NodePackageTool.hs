{-# LANGUAGE RecordWildCards, NoImplicitPrelude, LambdaCase, FlexibleContexts, NamedFieldPuns, OverloadedStrings #-}
import Protolude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Monad.Except as ExcT
import qualified Control.Exception as Exc
import qualified System.IO.Error as IOErr
import qualified Data.ByteString.Lazy as BL
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import qualified System.Posix.Files as PosixFiles
import Options.Applicative

import qualified Distribution.Nodejs.Package as NP
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

data Args
  = Args
    { argsPackageDir :: FilePath
    , argsMode :: Mode }

data Mode
  = LinkBin
    { linkBinTargetDir :: FilePath }
  | SetBinExecFlag

args :: Parser Args
args = subparser
    (  command "link-bin"
       (info (modeCommands linkBinSubcommands)
         (progDesc "link package dependecies’ bin files"))
    <> command "set-bin-exec-flag"
       (info (modeCommands setBinExecFlagSubcommands)
         (progDesc "make all bin scripts executable")) )
  where
    modeCommands modeSubcommands = Args
      <$> strOption
        ( long "package"
        <> metavar "FOLDER"
        <> help "folder with the node package" )
      <*> modeSubcommands
    linkBinSubcommands = LinkBin
      <$> strOption
        ( long "to"
        <> metavar "LINK_TARGET"
        <> help "folder to link to (absolute or relative from package folder)" )
    setBinExecFlagSubcommands = pure SetBinExecFlag


type ErrorLogger = ExceptT [Char] IO

-- | Print a warning to stdout.
warn :: [Text] -> ErrorLogger ()
warn ws = for_ ws $ \w -> liftIO $ TIO.hPutStrLn stderr ("Warning: " <> w)

-- | On Exception rethrow with annotation.
tryIOMsg :: ([Char] -> [Char]) -> IO a -> ErrorLogger a
tryIOMsg errAnn = ExcT.withExceptT (errAnn . Exc.displayException) . tryIO


main :: IO ()
main = execParser (info (args <**> helper)
                    (progDesc "Tool for various node package maintenance tasks"))
       >>= realMain

realMain :: Args -> IO ()
realMain Args{..} = do
  -- basic sanity checks
  let packageJsonPath = argsPackageDir FP.</> "package.json"
  unlessM (Dir.doesFileExist packageJsonPath)
    $ die $ toS $ packageJsonPath <> " does not exist."
  case argsMode of
    LinkBin{..} -> do
      unlessM (Dir.doesDirectoryExist linkBinTargetDir)
        $ die $ toS $ linkBinTargetDir <> " is not a directory."
    _ -> pass

  -- parse & decode & run logic
  runExceptT
    (tryRead packageJsonPath >>= tryDecode packageJsonPath >>= go)
    >>= \case
      (Left err) -> die $ toS $ "ERROR: " <> err
      (Right _) -> pass

  where
    tryRead :: FilePath -> ErrorLogger BL.ByteString
    tryRead fp = tryIOMsg exc $ BL.readFile fp
      where exc e = fp <> " cannot be read:\n" <> e
    tryDecode :: FilePath -> BL.ByteString -> ExceptT [Char] IO NP.Package
    tryDecode fp fileBs = do
      (pkg, warnings) <- NP.unLoggingPackage
                      <$> ExcT.ExceptT (pure $ first exc $ NP.decode fileBs)
      warn $ fmap ((\w -> toS fp <> ": " <> w) . NP.formatWarning) warnings
      pure pkg
      where exc e = fp <> " cannot be decoded\n" <> toS e

    tryAccess :: IO a -> IO (Maybe a)
    tryAccess io =
      hush <$> tryJust
        (\e -> guard (IOErr.isDoesNotExistError e ||
                      IOErr.isPermissionError e))
          io

    qte s = "\"" <> s <> "\""

    go :: NP.Package -> ErrorLogger ()
    go NP.Package{bin} = do
     binFiles <- readBinFiles bin
     for_ binFiles $ case argsMode of
        -- Link all dependency binaries to their target folder
        LinkBin{..} -> linkBin linkBinTargetDir
        -- Set the executable flag of all package binaries
        SetBinExecFlag -> setBinExecFlag . snd

    -- | Read the binary files and return their names & paths.
    readBinFiles :: NP.Bin -> ErrorLogger [(Text, FilePath)]
    readBinFiles bin = case bin of
      -- files with names how they should be linked
      (NP.BinFiles bs) -> pure $ (bs & KeyMap.toList <&> first Key.toText)
      -- a whole folder where everything should be linked
      (NP.BinFolder bf) -> do
        dirM <- liftIO $ tryAccess (Dir.listDirectory bf)
        case dirM of
          Nothing -> do
            warn ["Binary folder " <> toS (qte bf) <> " could not be accessed."]
            pure []
          (Just dir) ->
            pure $ fmap (\f -> (toS f, bf FP.</> f)) dir

    -- | Canonicalize the path.
    canon :: FilePath -> ErrorLogger FilePath
    canon fp = tryIOMsg
      (\e -> "Couldn’t canonicalize path " <> qte fp <> ": " <> e)
      (Dir.canonicalizePath fp)

    -- | Canonicalize relative to our package
    -- and ensure that the relative path is not outside.
    canonPkg :: FilePath -> ErrorLogger FilePath
    canonPkg relPath = do
      pkgDir <- canon argsPackageDir
      resPath <- canon $ argsPackageDir FP.</> relPath
      when (not $ pkgDir `isPrefixOf` resPath)
        $ throwError $ mconcat
          [ "The link to executable file "
          , qte relPath
          , " lies outside of the package folder!\n"
          , "That’s a security risk, aborting." ]
      pure resPath

    -- | Link a binary file to @targetDir/name@.
    -- @relBinPath@ is relative from the package dir.
    linkBin :: FilePath -> (Text, FilePath) -> ErrorLogger ()
    linkBin targetDir_ (name_, relBinPath) = do
      binPath <- canonPkg relBinPath
      (name, targetDir) <- traverse canon $
        symlinkTarget name_ targetDir_
      tryIOMsg (\e -> "Directory could not be created: " <> e) $
        Dir.createDirectoryIfMissing False targetDir
      tryIOMsg (\e -> "Symlink could not be created: " <> e) $
        PosixFiles.createSymbolicLink binPath $ targetDir FP.</> toS name

    -- | Given a name and a target directory, return
    --   the basename and the target (sub) directory
    --   of the target file
    symlinkTarget :: Text -> FilePath -> (FilePath, FilePath)
    symlinkTarget name targetDir =
      if "/" `T.isInfixOf` name
        then (FP.takeFileName name', targetDir FP.</> FP.takeDirectory name')
        else (name', targetDir)
      where name' = T.unpack name

    -- | Set executable flag of the file.
    setBinExecFlag :: FilePath -> ErrorLogger ()
    setBinExecFlag file_ = do
      file <- canonPkg file_
      res <- liftIO $ tryAccess $ do
        perm <- Dir.getPermissions file
        Dir.setPermissions file
          $ Dir.setOwnerExecutable True perm
      case res of
        Nothing ->
          warn ["Cannot set executable bit on " <> toS file]
        Just () -> pass
