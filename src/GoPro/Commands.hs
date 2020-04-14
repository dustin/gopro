{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module GoPro.Commands where

import           Control.Applicative     (Alternative (..), (<|>))
import           Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import           Control.Monad           (MonadPlus (..), mzero)
import           Control.Monad.Catch     (MonadCatch (..), MonadMask (..),
                                          MonadThrow (..), SomeException (..),
                                          bracket_, catch)
import           Control.Monad.Fail      (MonadFail (..))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Logger    (Loc (..), LogLevel (..), LogSource,
                                          LogStr, LoggingT, MonadLogger (..),
                                          ToLogStr (..), logDebugN, logErrorN,
                                          logInfoN, monadLoggerLog)
import           Control.Monad.Reader    (MonadReader, ReaderT (..), asks, lift)
import           Data.Cache              (Cache (..), fetchWithCache)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as T
import           Database.SQLite.Simple  (Connection)
import           Network.AWS.S3          (BucketName (..))
import           UnliftIO                (MonadUnliftIO (..), mapConcurrently)

import           GoPro.AuthDB
import           GoPro.DB                (HasGoProDB (..))
import           GoPro.Plus.Auth

data Options = Options
    { optDBPath              :: String
    , optStaticPath          :: FilePath
    , optVerbose             :: Bool
    , optUploadConcurrency   :: Int
    , optDownloadConcurrency :: Int
    , optArgv                :: [String]
    }

data Env = Env
    { gpOptions :: Options
    , dbConn    :: Connection
    , gpConfig  :: Map T.Text T.Text
    , authCache :: Cache () AuthInfo
    , envLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    }

gpBucket :: Env -> BucketName
gpBucket Env{gpConfig} = BucketName (Map.findWithDefault "" "bucket" gpConfig)

newtype EnvM a = EnvM
  { runEnvM :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO,
              MonadCatch, MonadThrow, MonadMask, MonadReader Env, MonadFail)

instance (Monad m, MonadLogger m, MonadIO m, MonadReader Env m) => HasGoProAuth m where
  goproAuth = asks authCache >>= \c -> fetchWithCache c () (\() -> logDebugN "Reading auth token from DB" >>
                                                                   asks dbConn >>= loadAuth)

instance (Monad m, MonadReader Env m) => HasGoProDB m where
  goproDB = asks dbConn

instance MonadLogger EnvM where
  monadLoggerLog loc src lvl msg = asks envLogger >>= \l -> liftIO $ l loc src lvl (toLogStr msg)

type GoPro = ReaderT Env (LoggingT IO)

instance MonadPlus (LoggingT IO) where
  mzero = lift mzero

instance Alternative (LoggingT IO) where
  empty = lift empty
  a <|> b = a `catch` \(SomeException _) -> b

mapConcurrentlyLimited :: (MonadMask m, MonadUnliftIO m, Traversable f, Foldable f)
                       => Int
                       -> (a -> m b)
                       -> f a
                       -> m (f b)
mapConcurrentlyLimited n f l = liftIO (newQSem n) >>= \q -> mapConcurrently (b q) l
  where b q x = bracket_ (liftIO (waitQSem q)) (liftIO (signalQSem q)) (f x)

logError :: MonadLogger m => T.Text -> m ()
logError = logErrorN

logInfo :: MonadLogger m => T.Text -> m ()
logInfo = logInfoN

logDbg :: MonadLogger m => T.Text -> m ()
logDbg = logDebugN

tshow :: Show a => a -> T.Text
tshow = T.pack . show

runIO :: Env -> EnvM a -> IO a
runIO e m = runReaderT (runEnvM m) e
