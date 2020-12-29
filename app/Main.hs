{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Control.Applicative                  ((<|>))
import           Control.Monad                        (unless)
import           Control.Monad.Catch                  (bracket_)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (asks)
import           Data.Foldable                        (fold)
import           Data.List                            (intercalate, sortOn)
import qualified Data.Text                            as T
import           Options.Applicative                  (Parser, action, argument, auto, command, completeWith,
                                                       customExecParser, eitherReader, fullDesc, help, helper, info,
                                                       long, many, metavar, option, prefs, progDesc, short, showDefault,
                                                       showHelpOnError, some, str, strOption, subparser, switch, value,
                                                       (<**>))
import           Options.Applicative.Help.Levenshtein (editDistance)
import           System.IO                            (hFlush, hGetEcho, hSetEcho, stdin, stdout)

import           GoPro.AuthDB
import           GoPro.Commands
import           GoPro.Commands.Backup
import           GoPro.Commands.Config
import           GoPro.Commands.Fixup
import           GoPro.Commands.Sync
import           GoPro.Commands.Upload
import           GoPro.Commands.Web
import qualified GoPro.DB                             as DB
import           GoPro.Plus.Auth
import           GoPro.Plus.Media

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> option auto (short 'u' <> long "upload-concurrency" <> showDefault <> value 3 <> help "Upload concurrency")
  <*> option auto (short 'd' <> long "download-concurrency" <> showDefault <> value 11 <> help "Download concurrency")
  <*> subparser ( command "auth" (info (pure AuthCmd) (progDesc "Authenticate to GoPro"))
                  <> command "reauth" (info (pure ReauthCmd) (progDesc "Refresh authentication credentials"))
                  <> command "sync" (info (pure SyncCmd) (progDesc "Sync recent data from GoPro Plus"))
                  <> command "refresh" (info refreshCmd (progDesc "Refresh individual media"))
                  <> command "createupload" (info createUpCmd (progDesc "Create an upload"))
                  <> command "upload" (info uploadCmd
                                       (progDesc "Optionally create an upload, then run all uploads"))
                  <> command "createmulti" (info createMultiCmd (progDesc "Create a multipart upload"))
                  <> command "fetchall" (info (pure FetchAllCmd) (progDesc "Fully sync all metadata"))
                  <> command "cleanup" (info (pure CleanupCmd) (progDesc "Clean up any outstanding ops"))
                  <> command "serve" (info (pure ServeCmd) (progDesc "Run the UI web server"))
                  <> command "wait" (info (pure WaitCmd) (progDesc "Wait for outstanding uploads to complete"))
                  <> command "backup" (info (pure BackupCmd) (progDesc "Backup all media to S3"))
                  <> command "processSQS" (info (pure ProcessSQSCmd) (progDesc "Process SQS queue"))
                  <> command "backuplocal" (info backupLocalCmd (progDesc "Backup all media to local path"))
                  <> command "config" (info configCmd (progDesc "Interact with config"))
                )
  where
    refreshCmd = RefreshCmd <$> some (argument str (metavar "mIDs..."))
    createUpCmd = CreateUploadCmd <$> some (argument str (metavar "file..." <> action "file"))
    uploadCmd = UploadCmd <$> many (argument str (metavar "file..." <> action "file"))
    createMultiCmd = CreateMultiCmd <$> argument mediumType (metavar "Mediumtype" <> completeWith mtypes)
                     <*> some (argument str (metavar "file..." <> action "file"))
    mediumType = eitherReader $ \s -> case reads s of
                                        [(x,_)] -> pure x
                                        _       -> Left (inv "MediumType" s mtypes)
    mtypes = [show t | t <- [minBound..] :: [MediumType]]

    backupLocalCmd = BackupLocalCmd <$> argument str (metavar "path" <> action "directory")

    configCmd = ConfigCmd . Just <$> argument cfgOpt (metavar "configopt" <> completeWith opttypes)
                <*> fmap exactlyOne (many (argument str (metavar "val")))
                <|> pure (ConfigCmd Nothing Nothing)
    cfgOpt = eitherReader $ \s -> maybe (Left (inv "config option" s opttypes)) Right (DB.strOption (T.pack s))
    opttypes = [T.unpack (DB.optionStr t) | t <- [minBound..] :: [DB.ConfigOption]]

    exactlyOne [a] = Just a
    exactlyOne _   = Nothing
    bestMatch n = head . sortOn (editDistance n)
    inv t v vs = fold ["invalid ", t, ": ", show v, ", perhaps you meant: ", bestMatch v vs,
                        "\nValid values:  ", intercalate ", " vs]

runCleanup :: GoPro ()
runCleanup = mapM_ rm =<< (filter wanted <$> listAll)
    where
      wanted Medium{..} = _medium_ready_to_view `elem` [ViewUploading, ViewFailure]
      rm Medium{..} = do
        liftIO . putStrLn $ "Removing " <> T.unpack _medium_id <> " (" <> show _medium_ready_to_view <> ")"
        errs <- delete _medium_id
        unless (null errs) . liftIO . putStrLn $ " error: " <> show errs

runAuth :: GoPro ()
runAuth = do
  u <- liftIO (prompt "Enter email: " >> getLine)
  p <- getPass
  db <- asks dbConn
  res <- authenticate u p
  updateAuth db res

  where
    prompt x = putStr x >> hFlush stdout
    withEcho echo a = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) a

    getPass = liftIO $ withEcho False getLine

runReauth :: GoPro ()
runReauth = do
  db <- asks dbConn
  res <- refreshAuth =<< loadAuth db
  updateAuth db res

run :: Command -> GoPro ()
run AuthCmd                       = runAuth
run ReauthCmd                     = runReauth
run SyncCmd                       = runFullSync
run (RefreshCmd mids)             = refreshMedia mids
run (CreateUploadCmd fs)          = runCreateUploads fs
run (CreateMultiCmd t fs)         = runCreateMultipart t fs
run (UploadCmd fs)                = runCreateUploads fs >> runResumeUpload
run FetchAllCmd                   = runFetch Full
run CleanupCmd                    = runCleanup
run (FixupCmd q)                  = runFixup q
run ServeCmd                      = runServer
run WaitCmd                       = runWaitForUploads
run BackupCmd                     = runBackup >> runReceiveS3CopyQueue
run ProcessSQSCmd                 = runReceiveS3CopyQueue
run (BackupLocalCmd p)            = runLocalBackup p
run (ConfigCmd Nothing Nothing)   = runListConfig
run (ConfigCmd (Just k) Nothing)  = runGetConfig k
run (ConfigCmd (Just k) (Just v)) = runSetConfig k v
run (ConfigCmd _ _)               = fail "invalid config command"

main :: IO ()
main = do
  o@Options{..} <- customExecParser (prefs showHelpOnError) opts
  runWithOptions o (run optCommand)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")
