{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Control.Monad                        (unless)
import           Control.Monad.Catch                  (bracket_)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (asks)
import           Data.List                            (intercalate, sortOn)
import qualified Data.Text                            as T
import           Options.Applicative                  (Parser, argument, auto, command, eitherReader, execParser,
                                                       fullDesc, help, helper, info, long, many, metavar, option,
                                                       progDesc, short, showDefault, some, str, strOption, subparser,
                                                       switch, value, (<**>))
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
    createUpCmd = CreateUploadCmd <$> some (argument str (metavar "file..."))
    uploadCmd = UploadCmd <$> many (argument str (metavar "file..."))
    createMultiCmd = CreateMultiCmd <$> argument mediumType (metavar "Mediumtype")
                     <*> some (argument str (metavar "file..."))
    backupLocalCmd = BackupLocalCmd <$> argument str (metavar "path")
    configCmd = ConfigCmd <$> many (argument str (metavar "args..."))
    mediumType = eitherReader $ \s -> case reads s of
                                        [(x,_)] -> pure x
                                        _ -> Left ("invalid MediumType: '" <> s
                                                   <> "', perhaps you meant: '" <> bestMatch s mtypes
                                                   <> "'\n All valid medium types include: " <>
                                                    intercalate ", " mtypes)
    bestMatch n = head . sortOn (editDistance n)
    mtypes = [show t | t <- [minBound..] :: [MediumType]]

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
    withEcho echo action = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    getPass = liftIO $ withEcho False getLine

runReauth :: GoPro ()
runReauth = do
  db <- asks dbConn
  res <- refreshAuth =<< loadAuth db
  updateAuth db res

run :: Command -> GoPro ()
run AuthCmd               = runAuth
run ReauthCmd             = runReauth
run SyncCmd               = runFullSync
run (RefreshCmd mids)     = refreshMedia mids
run (CreateUploadCmd fs)  = runCreateUploads fs
run (CreateMultiCmd t fs) = runCreateMultipart t fs
run (UploadCmd fs)        = runCreateUploads fs >> runResumeUpload
run FetchAllCmd           = runFetch Full
run CleanupCmd            = runCleanup
run (FixupCmd q)          = runFixup q
run ServeCmd              = runServer
run WaitCmd               = runWaitForUploads
run BackupCmd             = runBackup >> runReceiveS3CopyQueue
run ProcessSQSCmd         = runReceiveS3CopyQueue
run (BackupLocalCmd p)    = runLocalBackup p
run (ConfigCmd a)         = runConfig a

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  runWithOptions o (run optCommand)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")
