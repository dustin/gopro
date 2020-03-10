{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Main where

import           Control.Concurrent.Async      (mapConcurrently)
import           Control.Concurrent.QSem       (newQSem, signalQSem, waitQSem)
import           Control.Exception             (bracket_)
import           Control.Lens                  hiding (argument)
import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Logger          (Loc (..), LogLevel (..),
                                                LogSource, LogStr, LoggingT,
                                                MonadLogger (..),
                                                MonadLoggerIO (..),
                                                ToLogStr (..), filterLogger,
                                                logDebugN, logInfoN,
                                                monadLoggerLog,
                                                runStderrLoggingT)
import           Control.Monad.Reader          (MonadReader, ReaderT (..), ask,
                                                asks, lift, runReaderT)
import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as HM
import           Data.List.Extra               (chunksOf)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Vector                   as V
import           Database.SQLite.Simple        (Connection, withConnection)
import           GoPro
import           GoPro.AuthDB
import           GoPro.DB
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import           Options.Applicative           (Parser, argument, execParser,
                                                fullDesc, help, helper, info,
                                                long, metavar, progDesc, short,
                                                showDefault, some, str,
                                                strOption, switch, value,
                                                (<**>))
import           System.FilePath.Posix         ((</>))
import           System.IO                     (hFlush, hGetEcho, hSetEcho,
                                                stdin, stdout)
import           Web.Scotty.Trans              (ScottyT, file, get, json,
                                                middleware, param, raw, scottyT,
                                                setHeader)

data Options = Options {
  optDBPath     :: String,
  optStaticPath :: FilePath,
  optVerbose    :: Bool,
  optArgv       :: [String]
  }

data Env = Env {
  gpOptions :: Options,
  dbConn    :: Connection,
  envLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

newtype EnvM a = EnvM
  { runEnvM :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance MonadLogger EnvM where
  monadLoggerLog loc src lvl msg = do
    l <- asks envLogger
    liftIO $ l loc src lvl (toLogStr msg)

type GoPro = ReaderT Env (LoggingT IO)

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> some (argument str (metavar "cmd args..."))

logInfo :: MonadLogger m => T.Text -> m ()
logInfo = logInfoN

logDbg :: MonadLogger m => T.Text -> m ()
logDbg = logDebugN

tshow :: Show a => a -> T.Text
tshow = T.pack . show

mapConcurrentlyLimited :: (Traversable f, Foldable f) => Int -> (a -> IO b) -> f a -> IO (f b)
mapConcurrentlyLimited n f l = newQSem n >>= \q -> mapConcurrently (b q) l
  where b q x = bracket_ (waitQSem q) (signalQSem q) (f x)

data SyncType = Full | Incremental

runSync :: SyncType -> GoPro ()
runSync stype = do
  tok <- getToken
  db <- asks dbConn
  seen <- Set.fromList <$> loadMediaIDs db
  ms <- todo tok seen
  logInfo $ tshow (length ms) <> " new items"
  when (not . null $ ms) $ logDbg $ "new items: " <> tshow (ms ^.. folded . media_id)
  mapM_ (storeSome tok db) $ chunksOf 100 ms

    where resolve tok m = MediaRow m <$> fetchThumbnail tok m
          todo tok seen = filter (\m@Media{..} -> notSeen m && _media_ready_to_view == "ready")
                          <$> listWhile tok (listPred stype)
            where
              notSeen = (`Set.notMember` seen) . _media_id
              listPred Incremental = all notSeen
              listPred Full        = const True
          storeSome tok db l = do
            logInfo $ "Storing batch of " <> tshow (length l)
            storeMedia db =<< fetch tok l
          fetch tok = liftIO . mapConcurrentlyLimited 11 (resolve tok)

runAuth :: GoPro ()
runAuth = do
  liftIO (prompt "Enter email: ")
  u <- liftIO getLine
  p <- liftIO getPass
  db <- asks dbConn
  res <- authenticate u p
  updateAuth db res

  where
    prompt x = putStr x >> hFlush stdout
    withEcho echo action = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    getPass = withEcho False getLine

runReauth :: GoPro ()
runReauth = do
  db <- asks dbConn
  a <- loadAuth db
  res <- refreshAuth a
  updateAuth db res

getToken :: (MonadLogger m, MonadIO m, MonadReader Env m) => m String
getToken = do
  logDbg "Loading token"
  loadToken =<< asks dbConn

runServer :: GoPro ()
runServer = ask >>= \x -> scottyT 8008 (runIO x) application
  where
    runIO :: Env -> EnvM a -> IO a
    runIO e m = runReaderT (runEnvM m) e

    application :: ScottyT LT.Text EnvM ()
    application = do
      let staticPath = "static"
      middleware $ staticPolicy (noDots >-> addBase staticPath)

      get "/" $ do
        setHeader "Content-Type" "text/html"
        file $ staticPath </> "index.html"

      get "/api/media" $ do
        db <- lift $ asks dbConn
        json =<< loadMedia db

      get "/api/retrieve/:id" $ do
        imgid <- param "id"
        tok <- lift getToken
        setHeader "Content-Type" "application/json"
        raw =<< (lift $ proxy tok (dlURL imgid))

      get "/thumb/:id" $ do
        db <- lift $ asks dbConn
        imgid <- param "id"
        setHeader "Content-Type" "image/jpeg"
        setHeader "Cache-Control" "max-age=86400"
        raw =<< loadThumbnail db imgid

      get "/api/retrieve2/:id" $ do
        imgid <- param "id"
        tok <- lift getToken
        fi <- _fileStuff <$> retrieve tok imgid
        json (encd fi)
          where
            wh w h = T.pack (show w <> "x" <> show h)
            ts = J.String . T.pack
            encd FileStuff{..} = J.Array . V.fromList . fmap J.Object $ (
              map (\f -> HM.fromList [("url", ts (f ^. file_url)),
                                      ("name", ts "file"),
                                      ("desc", J.String $ wh (f ^. file_width) (f ^. file_height))]) _files
              <> map (\f -> HM.fromList [("url", ts (f ^. var_url)),
                                         ("name", ts (f ^. var_label)),
                                         ("desc", J.String $ "var " <> wh (f ^. var_width) (f ^. var_height))]) _variations
              )

run :: String -> GoPro ()
run "auth"     = runAuth
run "reauth"   = runReauth
run "sync"     = runSync Incremental
run "fullsync" = runSync Full
run "serve"    = runServer
run x          = fail ("unknown command: " <> x)

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  withConnection optDBPath (runConn o)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")

    runConn o@Options{..} db = do
      runStderrLoggingT . logfilt $ do
        l <- askLoggerIO
        runReaderT (run (head optArgv)) (Env o db l)

        where
          logfilt = filterLogger (\_ -> flip (if optVerbose then (>=) else (>)) LevelDebug)
