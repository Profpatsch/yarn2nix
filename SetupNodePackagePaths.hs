{-# LANGUAGE RecordWildCards, NoImplicitPrelude, LambdaCase, FlexibleContexts, NamedFieldPuns #-}
import Protolude
import qualified Data.Text.IO as TIO
import qualified Control.Monad.Except as ExcT
import qualified Control.Exception as Exc
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
    tryIOMsg :: ([Char] -> [Char]) -> IO a -> ExceptT [Char] IO a
    tryIOMsg errAnn = ExcT.withExceptT (errAnn . Exc.displayException) . tryIO

    tryRead :: FilePath -> ExceptT [Char] IO BL.ByteString
    tryRead fp = tryIOMsg exc $ BL.readFile fp
      where exc e = fp <> " cannot be read:\n" <> e
    tryDecode :: FilePath -> BL.ByteString -> ExceptT [Char] IO NP.Package
    tryDecode fp fileBs = do
      (pkg, warnings) <- NP.unLoggingPackage
                      <$> ExcT.ExceptT (pure $ first exc $ NP.decode fileBs)
      for_ warnings $ liftIO . TIO.hPutStrLn stderr . NP.formatWarning
      pure pkg
      where exc e = fp <> " cannot be decoded\n" <> toS e

    go NP.Package{bin} = case argMode of
      BinMode -> traverse linkBin =<< case bin of
        -- files with names how they should be linked
        (NP.BinFiles bs) -> pure $ HML.toList bs
        -- a whole folder where everything should be linked
        (NP.BinFolder bf) -> tryIOMsg
          (\e -> "could not list binary folder \"" <> bf <> "\": " <> e)
          (fmap (\f -> (toS f, bf FP.</> f)) <$> Dir.listDirectory bf)

    -- | Link a binary file to @targetDir/name@.
    -- @relBinPath@ is relative from the package dir.
    linkBin :: (Text, FilePath) -> ExceptT [Char] IO ()
    linkBin (name, relBinPath) = do
      let canon fp = tryIOMsg
            (\e -> "couldn’t canonicalize path \"" <> fp <> "\": " <> e)
            (Dir.canonicalizePath fp)
      pkgDir <- canon argPackageDir
      binPath <- canon $ argPackageDir FP.</> relBinPath
      targetDir <- canon argTargetDir
      print $ "pkgDir: " <> pkgDir
      print $ "binPath: " <> binPath
      print $ "relBinPath: " <> relBinPath
      print $ "targetDir: " <> targetDir
      when (not $ pkgDir `isPrefixOf` binPath)
        $ throwError $ mconcat
          [ "The link to executable file \""
          , relBinPath
          , "\" lies outside of the package folder!\n"
          , "That’s a security risk, aborting." ]
      tryIOMsg
        (\e -> "symlink could not be created: " <> e)
        (PosixFiles.createSymbolicLink binPath $ targetDir FP.</> toS name)
