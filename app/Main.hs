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
import           Data.Foldable                        (fold, traverse_)
import           Data.List                            (intercalate, sortOn)
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Text                            as T
import           Options.Applicative                  (Parser, ReadM, action, argument, auto, command, completeWith,
                                                       customExecParser, eitherReader, fullDesc, help, helper,
                                                       hsubparser, info, long, many, metavar, option, optional, prefs,
                                                       progDesc, readerError, short, showDefault, showHelpOnError, some,
                                                       str, strOption, switch, value, (<**>))
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

atLeast :: (Read n, Show n, Ord n, Num n) => n -> ReadM n
atLeast n = auto >>= \i -> if i >= n then pure i else readerError ("must be at least " <> show n)

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> option auto (short 'u' <> long "upload-concurrency" <> showDefault <> value 3 <> help "Upload concurrency")
  <*> option auto (short 'd' <> long "download-concurrency" <> showDefault <> value 11 <> help "Download concurrency")
  <*> option (atLeast (5*1024*1024)) (short 's' <> long "chunk-size"
                                      <> showDefault <> value (6*1024*1024) <> help "Upload chunk size.")
  <*> (optional $ strOption (long "refdir" <> help "download reference directory"))
  <*> hsubparser (command "auth" (info (pure AuthCmd) (progDesc "Authenticate to GoPro"))
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
                  <> command "reprocess" (info reprocessCmd (progDesc "Reprocess failed uploads"))
                  <> command "backup" (info (pure $ BackupCmd extractOrig) (progDesc "Backup original media to S3"))
                  <> command "clearmeta" (info (pure ClearMetaCmd) (progDesc "Remove local, backed-up metadata"))
                  <> command "backupall" (info (pure $ BackupCmd extractMedia) (progDesc "Backup all media to S3"))
                  <> command "processSQS" (info (pure ProcessSQSCmd) (progDesc "Process SQS queue"))
                  <> command "backuplocal" (info backupLocalCmd (progDesc "Backup original media to local path"))
                  <> command "backuplocalall" (info backupLocalAllCmd (progDesc "Backup all media to local path"))
                  <> command "fixup" (info fixupCmd (progDesc "Update GoPro cloud properties via SQL"))
                  <> command "config" (info configCmd (progDesc "Interact with config"))
                 )
  where
    refreshCmd = RefreshCmd <$> some1 (argument str (metavar "mIDs..."))
    reprocessCmd = ReprocessCmd <$> some1 (argument str (metavar "mIDs..."))
    createUpCmd = CreateUploadCmd <$> some1 (argument str (metavar "file..." <> action "file"))
    uploadCmd = UploadCmd <$> many (argument str (metavar "file..." <> action "file"))
    createMultiCmd = CreateMultiCmd <$> argument mediumType (metavar "Mediumtype" <> completeWith mtypes)
                     <*> some1 (argument str (metavar "file..." <> action "file"))
    mediumType = eitherReader $ \s -> case reads s of
                                        [(x,_)] -> pure x
                                        _       -> Left (inv "MediumType" s mtypes)
    mtypes = [show t | t <- [minBound..] :: [MediumType]]

    backupLocalCmd = BackupLocalCmd extractOrig <$> argument str (metavar "path" <> action "directory")
    backupLocalAllCmd = BackupLocalCmd extractMedia <$> argument str (metavar "path" <> action "directory")

    configCmd = hsubparser (foldMap optCmd [minBound ..]) <|> pure ConfigListCmd

    fixupCmd = FixupCmd <$> argument str (metavar "query")

    optCmd o = command (T.unpack (DB.optionStr o))
               (info opt (progDesc ("get/set " <> T.unpack (DB.optionStr o) <> " config")))

      where
        opt = ConfigSetCmd o <$> argument str (metavar "val")
                 <|> pure (ConfigGetCmd o)

    bestMatch n = head . sortOn (editDistance n)
    inv t v vs = fold ["invalid ", t, ": ", show v, ", perhaps you meant: ", bestMatch v vs,
                        "\nValid values:  ", intercalate ", " vs]

some1 :: Parser a -> Parser (NonEmpty a)
some1 p = NE.fromList <$> some p

runCleanup :: GoPro ()
runCleanup = DB.clearUploads *> (mapM_ rm =<< (filter wanted <$> notReady))
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
  res <- refreshAuth . arInfo =<< loadAuth db
  updateAuth db res

run :: Command -> GoPro ()
run AuthCmd               = runAuth
run ReauthCmd             = runReauth
run SyncCmd               = runFullSync
run (RefreshCmd mids)     = refreshMedia mids
run (CreateUploadCmd fs)  = runCreateUploads fs
run (CreateMultiCmd t fs) = runCreateMultipart t fs
run (UploadCmd fs)        = ifne fs runCreateUploads >> runResumeUpload
run FetchAllCmd           = runFetch Full
run CleanupCmd            = runCleanup
run (FixupCmd q)          = runFixup q
run ServeCmd              = runServer
run WaitCmd               = runWaitForUploads
run (ReprocessCmd ms)     = runReprocessCmd ms
run (BackupCmd x)         = runBackup x >> runReceiveS3CopyQueue
run ProcessSQSCmd         = runReceiveS3CopyQueue
run (BackupLocalCmd x p)  = runLocalBackup x p
run ConfigListCmd         = runListConfig
run (ConfigGetCmd k)      = runGetConfig k
run (ConfigSetCmd k v)    = runSetConfig k v
run ClearMetaCmd          = runClearMeta

-- Perform an action on a list if the list is non-empty
ifne :: Monad m => [a] -> (NonEmpty a -> m ()) -> m ()
ifne l a = traverse_ a (NE.nonEmpty l)

main :: IO ()
main = do
  o@Options{..} <- customExecParser (prefs showHelpOnError) opts
  runWithOptions o (run optCommand)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")
