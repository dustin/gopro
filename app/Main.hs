{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           Cleff
import           Cleff.Fail
import           Cleff.Reader
import           Control.Applicative                  ((<|>))
import           Control.Monad                        (unless)
import           Control.Monad.Catch                  (bracket_)
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
import           System.Directory                     (getHomeDirectory)
import           System.FilePath.Posix                ((</>))
import           System.IO                            (hFlush, hGetEcho, hSetEcho, stdin, stdout)

import           GoPro.AuthCache
import           GoPro.Commands
import           GoPro.Commands.Backup
import           GoPro.Commands.Config
import           GoPro.Commands.Fixup
import           GoPro.Commands.Sync
import           GoPro.Commands.Upload
import           GoPro.Commands.Web
import           GoPro.Config.ConfigFile
import           GoPro.Config.Effect
import           GoPro.DB
import           GoPro.Logging
import           GoPro.Notification
import           GoPro.Plus.Auth
import           GoPro.Plus.Media
import           GoPro.S3

atLeast :: (Read n, Show n, Ord n, Num n) => n -> ReadM n
atLeast n = auto >>= \i -> if i >= n then pure i else readerError ("must be at least " <> show n)

options :: Options -> Parser Options
options Options{..} = Options
  <$> Options.Applicative.strOption (long "db" <> showDefault <> value optDBPath <> help "db path")
  <*> Options.Applicative.strOption (long "static" <> showDefault <> value optStaticPath <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> option auto (short 'u' <> long "upload-concurrency" <> showDefault <> value optUploadConcurrency <> help "Upload concurrency")
  <*> option auto (short 'd' <> long "download-concurrency" <> showDefault <> value optDownloadConcurrency <> help "Download concurrency")
  <*> option (atLeast (5*1024*1024)) (short 's' <> long "chunk-size"
                                      <> showDefault <> value optChunkSize <> help "Upload chunk size.")
  <*> optional (Options.Applicative.strOption (long "refdir" <> maybe mempty value optReferenceDir <> help "download reference directory"))
  <*> hsubparser (command "auth" (info (pure AuthCmd) (progDesc "Authenticate to GoPro"))
                  <> command "reauth" (info (pure ReauthCmd) (progDesc "Refresh authentication credentials"))
                  <> command "sync" (info (pure SyncCmd) (progDesc "Sync recent data from GoPro Plus"))
                  <> command "refresh" (info refreshCmd (progDesc "Refresh individual media"))
                  <> command "createupload" (info createUpCmd (progDesc "Create an upload"))
                  <> command "upload" (info uploadCmd
                                       (progDesc "Optionally create an upload, then run all uploads"))
                  <> command "download" (info downloadCmd (progDesc "Download media to local path"))
                  <> command "createmulti" (info createMultiCmd (progDesc "Create a multipart upload"))
                  <> command "fetchall" (info (pure FetchAllCmd) (progDesc "Fully sync all metadata"))
                  <> command "removedeleted" (info (pure RemoveDeleted) (progDesc "Delete local media missing from remote"))
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

    backupLocalCmd = BackupLocalCmd extractOrig <$> some1 (argument str (metavar "path" <> action "directory"))
    backupLocalAllCmd = BackupLocalCmd extractMedia <$> some1 (argument str (metavar "path" <> action "directory"))
    downloadCmd = DownloadCmd extractMedia
                    <$> argument str (metavar "path" <> action "directory")
                    <*> some1 (argument str (metavar "mIDs..."))

    configCmd = hsubparser (foldMap optCmd [minBound ..]) <|> pure ConfigListCmd

    fixupCmd = FixupCmd <$> argument str (metavar "query")

    optCmd o = command (T.unpack (optionStr o))
               (info opt (progDesc ("get/set " <> T.unpack (optionStr o) <> " config")))

      where
        opt = ConfigSetCmd o <$> argument str (metavar "val")
                 <|> pure (ConfigGetCmd o)

    bestMatch n = head . sortOn (editDistance n)
    inv t v vs = fold ["invalid ", t, ": ", show v, ", perhaps you meant: ", bestMatch v vs,
                        "\nValid values:  ", intercalate ", " vs]

some1 :: Parser a -> Parser (NonEmpty a)
some1 p = NE.fromList <$> some p

runCleanup :: [Reader Options, AuthCache, LogFX, DB, IOE] :>> es => Eff es ()
runCleanup = clearUploads *> (mapM_ rm . filter wanted =<< notReady)
  where
    wanted Medium{..} = _medium_ready_to_view `elem` [ViewRegistered, ViewUploading, ViewFailure]
    rm Medium{..} = do
      liftIO . putStrLn $ "Removing " <> T.unpack _medium_id <> " (" <> show _medium_ready_to_view <> ")"
      errs <- delete _medium_id
      unless (null errs) . liftIO . putStrLn $ " error: " <> show errs

runAuth :: [DB, IOE] :>> es => Eff es ()
runAuth = do
  u <- liftIO (prompt "Enter email: " >> getLine)
  p <- getPass
  updateAuth =<< authenticate u p

  where
    prompt x = putStr x >> hFlush stdout
    withEcho echo a = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) a

    getPass = liftIO $ withEcho False getLine

runReauth :: [DB, IOE] :>> es => Eff es ()
runReauth = updateAuth =<< refreshAuth . arInfo =<< loadAuth

run :: [Reader Options, AuthCache, ConfigFX, NotifyFX, LogFX, S3, DB, Fail, IOE] :>> es => Command -> Eff es ()
run AuthCmd               = runAuth
run ReauthCmd             = runReauth
run SyncCmd               = runFullSync
run (RefreshCmd mids)     = refreshMedia mids
run (CreateUploadCmd fs)  = runCreateUploads fs
run (CreateMultiCmd t fs) = runCreateMultipart t fs
run (UploadCmd fs)        = ifne fs runCreateUploads >> runResumeUpload
run FetchAllCmd           = runFetch Full
run RemoveDeleted         = removeDeleted
run CleanupCmd            = runCleanup
run (FixupCmd q)          = runFixup q
run ServeCmd              = runServer
run WaitCmd               = runWait
run (ReprocessCmd ms)     = runReprocessCmd ms
run (BackupCmd x)         = runBackup x >> runReceiveS3CopyQueue
run ProcessSQSCmd         = runReceiveS3CopyQueue
run (BackupLocalCmd x p)  = runLocalBackup x p
run (DownloadCmd x p l)   = runDownload x (pure p) l
run ConfigListCmd         = runListConfig
run (ConfigGetCmd k)      = runGetConfig k
run (ConfigSetCmd k v)    = runSetConfig k v
run ClearMetaCmd          = runClearMeta

-- Perform an action on a list if the list is non-empty
ifne :: Monad m => [a] -> (NonEmpty a -> m ()) -> m ()
ifne l a = traverse_ a (NE.nonEmpty l)

main :: IO ()
main = do
  homeConfig <- loadConfigFile defaultOptions . (</> ".config/gopro/config.toml") =<< getHomeDirectory
  localConfig <- loadConfigFile homeConfig ".gopro.toml"
  o@Options{..} <- customExecParser (prefs showHelpOnError) (opts localConfig)
  runWithOptions o (run optCommand)

  where
    opts def = info (options def <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")
