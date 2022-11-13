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
import           Control.Monad.Catch     (MonadCatch (..), MonadMask (..), MonadThrow (..), SomeException (..), catch)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Logger    (Loc (..), LogLevel (..), LogSource, LogStr, MonadLogger (..), ToLogStr (..),
                                          logDebugN, logErrorN, logInfoN, monadLoggerLog)
import           Control.Monad.Reader    (MonadReader, ReaderT (..), asks)
import           Data.Cache              (Cache (..), fetchWithCache, newCache)
import           Data.Foldable           (fold)
import           Data.List               (isPrefixOf)
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import           System.Clock            (TimeSpec (..))
import           UnliftIO                (MonadUnliftIO (..), bracket_, mapConcurrently, mapConcurrently_)

import           GoPro.DB                (AuthResult (..), ConfigOption (..), Database (..))
import           GoPro.DB.Postgres       (withPostgres)
import           GoPro.DB.Sqlite         (withSQLite)
import           GoPro.Logging
import           GoPro.Notification
import           GoPro.Plus.Auth
import           GoPro.Plus.Media        (FileInfo, MediumID, MediumType)
import           UnliftIO.MVar           (MVar, newEmptyMVar, putMVar, takeMVar)

-- Extractor function for deciding which files to download for backing up.
-- (medium id, head url, url)
type Extractor = MediumID -> FileInfo -> [(T.Text, String, String)]

data Command = AuthCmd
             | ReauthCmd
             | SyncCmd
             | RefreshCmd (NonEmpty MediumID)
             | CreateUploadCmd (NonEmpty FilePath)
             | UploadCmd [FilePath]
             | CreateMultiCmd MediumType (NonEmpty FilePath)
             | FetchAllCmd
             | CleanupCmd
             | FixupCmd T.Text
             | ServeCmd
             | WaitCmd
             | ReprocessCmd (NonEmpty MediumID)
             | BackupCmd Extractor
             | ProcessSQSCmd
             | BackupLocalCmd Extractor FilePath
             | DownloadCmd Extractor FilePath (NonEmpty MediumID)
             | ConfigListCmd
             | ConfigGetCmd ConfigOption
             | ConfigSetCmd ConfigOption T.Text
             | ClearMetaCmd

instance Show Command where show = const "[command]"

data Options = Options
    { optDBPath              :: String
    , optStaticPath          :: FilePath
    , optVerbose             :: Bool
    , optUploadConcurrency   :: Int
    , optDownloadConcurrency :: Int
    , optChunkSize           :: Integer
    , optReferenceDir        :: Maybe FilePath
    , optCommand             :: Command
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optDBPath = "gopro.db"
    , optStaticPath = "static"
    , optVerbose = False
    , optUploadConcurrency = 3
    , optDownloadConcurrency = 11
    , optChunkSize = 6*1024*1024
    , optReferenceDir = Nothing
    , optCommand  = AuthCmd -- this should not be used
    }

data Env = Env
    { gpOptions  :: Options
    , database   :: Database
    , gpConfig   :: Map ConfigOption T.Text
    , authCache  :: Cache () AuthInfo
    , authMutex  :: MVar ()
    , noteChan   :: TChan Notification
    , envLoggers :: [Loc -> LogSource -> LogLevel -> LogStr -> IO ()]
    }

asksOpt :: MonadReader Env m => (Options -> b) -> m b
asksOpt f = asks (f . gpOptions)

configItem :: ConfigOption -> Env -> T.Text
configItem k = configItemDef k ""

configItemDef :: ConfigOption -> T.Text -> Env -> T.Text
configItemDef k def Env{gpConfig} = Map.findWithDefault def k gpConfig

newtype GoProT m a = GoPro
  { runGoPro :: ReaderT Env m a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO,
              MonadCatch, MonadThrow, MonadMask, MonadReader Env, MonadFail)

type GoPro = GoProT IO

instance (Monad m, MonadLogger m, MonadUnliftIO m, MonadReader Env m) => HasGoProAuth m where
  goproAuth = authMutexed $ asks authCache >>= \c -> fetchWithCache c () (const fetchOrRefresh)

authMutexed :: (Monad m, MonadUnliftIO m, MonadReader Env m) => m a -> m a
authMutexed a = asks authMutex >>= \mutex -> bracket_ (putMVar mutex ()) (takeMVar mutex) a

fetchOrRefresh :: (Monad m, MonadLogger m, MonadIO m, MonadReader Env m) => m AuthInfo
fetchOrRefresh = asks database >>= \Database{..} -> do
  logDebugN "Reading auth token from DB"
  AuthResult ai expired <- loadAuth
  if expired then do
    logDebugN "Refreshing auth info"
    res <- refreshAuth ai
    updateAuth res
    pure res
  else
    pure ai

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
logDbgL = logDebugN . fold

tshow :: Show a => a -> T.Text
tshow = T.pack . show

runIO :: Env -> GoPro a -> IO a
runIO e m = runReaderT (runGoPro m) e

sendNotification :: Notification -> GoPro ()
sendNotification note = asks noteChan >>= \ch -> liftIO . atomically . writeTChan ch $ note

withDB :: String -> (Database -> IO a) -> IO a
withDB s
  | "postgres:" `isPrefixOf` s = withPostgres s
  | otherwise = withSQLite s

runWithOptions :: Options -> GoPro a -> IO a
runWithOptions o@Options{..} a = withDB optDBPath $ \d -> do
  initTables d
  cfg <- loadConfig d
  cache <- liftIO $ newCache (Just (TimeSpec 60 0))
  tc <- liftIO $ mkLogChannel
  mut <- newEmptyMVar
  let notlog = notificationLogger tc
      minLvl = if optVerbose then LevelDebug else LevelInfo
  liftIO $ runIO (Env o d cfg cache mut tc [baseLogger minLvl, notlog]) a
