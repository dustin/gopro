{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module GoPro.Commands where

import           Cleff
import           Cleff.Fail
import           Cleff.Reader
import           Control.Concurrent.STM (TChan, atomically, writeTChan)
import           Data.Cache             (Cache (..), fetchWithCache, newCache)
import           Data.List              (isPrefixOf)
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           System.Clock           (TimeSpec (..))
import           UnliftIO               (bracket_)
import           UnliftIO.MVar          (MVar, newEmptyMVar, putMVar, takeMVar)

import           GoPro.DB
import           GoPro.DB.Postgres      (runDatabasePostgresStr)
import           GoPro.DB.Sqlite        (runDatabaseSqliteStr)
import           GoPro.Logging
import           GoPro.Notification
import           GoPro.Plus.Auth
import           GoPro.Plus.Media       (FileInfo, MediumID, MediumType)

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
             | BackupLocalCmd Extractor (NonEmpty FilePath)
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
    { gpOptions :: Options
    , gpConfig  :: Map ConfigOption T.Text
    , authCache :: Cache () AuthInfo
    , authMutex :: MVar ()
    , noteChan  :: TChan Notification
    }

asksOpt :: Reader Env :> es => (Options -> b) -> Eff es b
asksOpt f = asks (f . gpOptions)

configItem :: ConfigOption -> Env -> T.Text
configItem k = configItemDef k ""

configItemDef :: ConfigOption -> T.Text -> Env -> T.Text
configItemDef k def Env{gpConfig} = Map.findWithDefault def k gpConfig

instance ([LogFX, IOE, DatabaseEff, Reader Env] :>> es) => HasGoProAuth (Eff es) where
  goproAuth = authMutexed $ asks authCache >>= \c -> fetchWithCache c () (const fetchOrRefresh)

authMutexed :: ([IOE, Reader Env] :>> es) => Eff es a -> Eff es a
authMutexed a = asks authMutex >>= \mutex -> bracket_ (putMVar mutex ()) (takeMVar mutex) a

fetchOrRefresh :: ([DatabaseEff, LogFX, IOE, Reader Env] :>> es) => Eff es AuthInfo
fetchOrRefresh = do
  logDbg "Reading auth token from DB"
  AuthResult ai expired <- loadAuth
  if expired then do
    logDbg "Refreshing auth info"
    res <- refreshAuth ai
    updateAuth res
    pure res
  else
    pure ai

tshow :: Show a => a -> T.Text
tshow = T.pack . show

sendNotification :: [IOE, Reader Env] :>> es => Notification -> Eff es ()
sendNotification note = asks noteChan >>= \ch -> liftIO . atomically . writeTChan ch $ note

withDB :: IOE :> es => String -> (Eff (DatabaseEff : es) a) -> Eff es a
withDB s
  | "postgres:" `isPrefixOf` s = runDatabasePostgresStr s
  | otherwise = runDatabaseSqliteStr s

runWithOptions :: Options -> (forall es. [IOE, DatabaseEff, LogFX, Fail, Reader Env] :>> es => Eff es a) -> IO a
runWithOptions o@Options{..} a = runIOE . runFailIO $ withDB optDBPath $ do
  initTables
  cfg <- loadConfig
  cache <- liftIO $ newCache (Just (TimeSpec 60 0))
  mut <- newEmptyMVar
  tc <- liftIO mkLogChannel
  runReader (Env o cfg cache mut tc) . runLogFX tc optVerbose $ a

