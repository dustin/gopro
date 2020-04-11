{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Monad          (unless)
import           Control.Monad.Catch    (bracket_)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger   (LogLevel (..), MonadLoggerIO (..),
                                         filterLogger, runStderrLoggingT)
import           Control.Monad.Reader   (ReaderT (..), asks, runReaderT)
import           Data.Cache             (newCache)
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Database.SQLite.Simple (withConnection)
import           Options.Applicative    (Parser, argument, auto, execParser,
                                         fullDesc, help, helper, info, long,
                                         metavar, option, progDesc, short,
                                         showDefault, some, str, strOption,
                                         switch, value, (<**>))
import           System.Clock           (TimeSpec (..))
import           System.IO              (hFlush, hGetEcho, hSetEcho, stdin,
                                         stdout)

import           GoPro.AuthDB
import           GoPro.Commands
import           GoPro.Commands.Backup
import           GoPro.Commands.Fixup
import           GoPro.Commands.Sync
import           GoPro.Commands.Upload
import           GoPro.Commands.Web
import           GoPro.DB
import           GoPro.Plus.Auth
import           GoPro.Plus.Media

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> option auto (short 'u' <> long "upload-concurrency" <> showDefault <> value 3 <> help "Upload concurrency")
  <*> option auto (short 'd' <> long "download-concurrency" <> showDefault <> value 11 <> help "Download concurrency")
  <*> strOption (long "bucket" <> showDefault <> value "gopro.bucket" <> help "S3 bucket")
  <*> some (argument str (metavar "cmd args..."))

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

run :: String -> GoPro ()
run c = fromMaybe (liftIO unknown) $ lookup c cmds
  where
    cmds = [("auth", runAuth),
            ("reauth", runReauth),
            ("sync", runWaitForTranscoding >> runFetch Incremental >> runGetMeta >> runGrokTel >> runGetMoments >> runStoreMeta),
            ("fetch", runFetch Incremental),
            ("upload", runUploadFiles),
            ("uploadmulti", runUploadMultipart),
            ("resumeupload", runResumeUpload),
            ("fetchall", runFetch Full),
            ("cleanup", runCleanup),
            ("fixup", runFixup),
            ("serve", runServer),
            ("getmeta", runGetMeta),
            ("groktel", runGrokTel),
            ("transcoding", runWaitForTranscoding),
            ("storemeta", runStoreMeta),
            ("backup", runBackup)]
    unknown = do
      putStrLn $ "Unknown command: " <> c
      putStrLn "Try one of these:"
      putStrLn $ "    " <> intercalate "\n    " (map fst cmds)

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  withConnection optDBPath (runConn o)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")

    runConn o@Options{..} db = do
      initTables db
      cache <- newCache (Just (TimeSpec 60 0))

      runStderrLoggingT . logfilt $ do
        l <- askLoggerIO
        let o' = o{optArgv = tail optArgv}
        runReaderT (run (head optArgv)) (Env o' db cache l)

        where
          logfilt = filterLogger (\_ -> flip (if optVerbose then (>=) else (>)) LevelDebug)
