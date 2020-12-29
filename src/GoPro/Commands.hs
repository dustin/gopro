{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module GoPro.Commands where

import           Control.Applicative     (Alternative (..), (<|>))
import           Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import           Control.Concurrent.STM  (TChan, atomically, writeTChan)
import           Control.Monad           (MonadPlus (..), mzero)
import           Control.Monad.Catch     (MonadCatch (..), MonadMask (..), MonadThrow (..), SomeException (..),
                                          bracket_, catch)
import           Control.Monad.Fail      (MonadFail (..))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Logger    (Loc (..), LogLevel (..), LogSource, LogStr, MonadLogger (..), ToLogStr (..),
                                          logDebugN, logErrorN, logInfoN, monadLoggerLog)
import           Control.Monad.Reader    (MonadReader, ReaderT (..), asks)
import           Data.Cache              (Cache (..), fetchWithCache, newCache)
import           Data.Foldable           (fold)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import           Database.SQLite.Simple  (Connection, Query, withConnection)
import           System.Clock            (TimeSpec (..))
import           UnliftIO                (MonadUnliftIO (..), mapConcurrently, mapConcurrently_)

import           GoPro.AuthDB
import           GoPro.DB                (HasGoProDB (..), initTables, loadConfig)
import           GoPro.Logging
import           GoPro.Notification
import           GoPro.Plus.Auth
import           GoPro.Plus.Media        (MediumID, MediumType)

data Command = AuthCmd
             | ReauthCmd
             | SyncCmd
             | RefreshCmd [MediumID]
             | CreateUploadCmd [FilePath]
             | UploadCmd [FilePath]
             | CreateMultiCmd MediumType [FilePath]
             | FetchAllCmd
             | CleanupCmd
             | FixupCmd Query
             | ServeCmd
             | WaitCmd
             | BackupCmd
             | ProcessSQSCmd
             | BackupLocalCmd FilePath
             | ConfigCmd [String]

data Options = Options
    { optDBPath              :: String
    , optStaticPath          :: FilePath
    , optVerbose             :: Bool
    , optUploadConcurrency   :: Int
    , optDownloadConcurrency :: Int
    , optCommand             :: Command
    }

data Env = Env
    { gpOptions  :: Options
    , dbConn     :: Connection
    , gpConfig   :: Map T.Text T.Text
    , authCache  :: Cache () AuthInfo
    , noteChan   :: TChan Notification
    , envLoggers :: [Loc -> LogSource -> LogLevel -> LogStr -> IO ()]
    }

configItem :: T.Text -> Env -> T.Text
configItem k = configItemDef k ""

configItemDef :: T.Text -> T.Text -> Env -> T.Text
configItemDef k def Env{gpConfig} = Map.findWithDefault def k gpConfig

newtype GoPro a = GoPro
  { runGoPro :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO,
              MonadCatch, MonadThrow, MonadMask, MonadReader Env, MonadFail)

instance (Monad m, MonadLogger m, MonadIO m, MonadReader Env m) => HasGoProAuth m where
  goproAuth = asks authCache >>= \c -> fetchWithCache c () (\() -> logDebugN "Reading auth token from DB" >>
                                                                   asks dbConn >>= loadAuth)

instance (Monad m, MonadReader Env m) => HasGoProDB m where
  goproDB = asks dbConn

instance MonadLogger GoPro where
  monadLoggerLog loc src lvl msg = mapM_ (\l -> liftIO $ l loc src lvl (toLogStr msg)) =<< asks envLoggers

instance MonadPlus GoPro where
  mzero = error "GoPro zero"

instance Alternative GoPro where
  empty = mzero
  a <|> b = a `catch` \(SomeException _) -> b

mapConcurrentlyLimited :: (MonadMask m, MonadUnliftIO m, Traversable f)
                       => Int
                       -> (a -> m b)
                       -> f a
                       -> m (f b)
mapConcurrentlyLimited n f l = liftIO (newQSem n) >>= \q -> mapConcurrently (b q) l
  where b q x = bracket_ (liftIO (waitQSem q)) (liftIO (signalQSem q)) (f x)

mapConcurrentlyLimited_ :: (MonadMask m, MonadUnliftIO m, Traversable f)
                        => Int
                        -> (a -> m b)
                        -> f a
                        -> m ()
mapConcurrentlyLimited_ n f l = liftIO (newQSem n) >>= \q -> mapConcurrently_ (b q) l
  where b q x = bracket_ (liftIO (waitQSem q)) (liftIO (signalQSem q)) (f x)

logError, logInfo, logDbg :: MonadLogger m => T.Text -> m ()

logError = logErrorN
logInfo = logInfoN
logDbg = logDebugN

logErrorL, logInfoL, logDbgL :: (Foldable f, MonadLogger m) => f T.Text-> m ()

logErrorL = logErrorN . fold
logInfoL = logInfoN . fold
logDbgL = logInfoN . fold

tshow :: Show a => a -> T.Text
tshow = T.pack . show

runIO :: Env -> GoPro a -> IO a
runIO e m = runReaderT (runGoPro m) e

sendNotification :: Notification -> GoPro ()
sendNotification note = asks noteChan >>= \ch -> liftIO . atomically . writeTChan ch $ note

runWithOptions :: Options -> GoPro a -> IO a
runWithOptions o@Options{..} a = withConnection optDBPath $ \db -> do
  initTables db
  cfg <- loadConfig db
  cache <- newCache (Just (TimeSpec 60 0))
  tc <- mkLogChannel
  let notlog = notificationLogger tc
      minLvl = if optVerbose then LevelDebug else LevelInfo
  liftIO $ runIO (Env o db cfg cache tc [baseLogger minLvl, notlog]) a
