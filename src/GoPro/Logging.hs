{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GoPro.Logging where

import           Cleff
import           Control.Concurrent.STM (TChan, atomically, newBroadcastTChanIO, writeTChan)
import           Control.Monad          (when)
import           Control.Monad.Logger   (Loc (..), LogLevel (..), LogSource, LogStr, MonadLogger (..), defaultLoc,
                                         fromLogStr, toLogStr)
import qualified Data.ByteString.Char8  as C8
import           Data.Foldable          (fold)
import           Data.String            (fromString)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           System.IO              (stderr)

import           GoPro.Notification

data LogFX :: Effect where
  LogFX :: Loc -> LogSource -> LogLevel -> LogStr -> LogFX m ()

makeEffect ''LogFX

instance (LogFX :> es) => MonadLogger (Eff es) where
  monadLoggerLog loc src lvl msg = send (LogFX loc src lvl (toLogStr msg))

genericLog :: LogLevel -> LogFX :> es => T.Text -> Eff es ()
genericLog lvl = logFX defaultLoc "" lvl . fromString . T.unpack

logError, logInfo, logDbg :: LogFX :> es => T.Text -> Eff es ()

logErrorL, logInfoL, logDbgL :: (Foldable f, LogFX :> es) => f T.Text -> Eff es ()

logErrorL = logError . fold
logInfoL = logInfo . fold
logDbgL = logDbg . fold

logError = genericLog LevelError

logInfo = genericLog LevelInfo

logDbg = genericLog LevelDebug

baseLogger :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
baseLogger minLvl _ _ lvl s = when (lvl >= minLvl) $ C8.hPutStrLn stderr (fromLogStr ls)
  where
    ls = prefix <> ": " <> s
    prefix = case lvl of
               LevelDebug   -> "D"
               LevelInfo    -> "I"
               LevelWarn    -> "W"
               LevelError   -> "E"
               LevelOther x -> fromString . T.unpack $ x

notificationLogger :: TChan Notification -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
notificationLogger ch _ _ lvl str = case lvl of
                                      LevelDebug -> pure ()
                                      LevelInfo  -> note NotificationInfo
                                      _          -> note NotificationError
  where note t = atomically $ writeTChan ch (Notification t "GoPro" lstr)
        lstr = TE.decodeUtf8 $ fromLogStr str

mkLogChannel :: IO (TChan Notification)
mkLogChannel = newBroadcastTChanIO

runLogFX :: (IOE :> es) => TChan Notification -> Bool -> Eff (LogFX : es) a -> Eff es a
runLogFX tc verbose = interpretIO \case
  LogFX loc src lvl' msg -> liftIO $ do
    baseLogger minLvl loc src lvl' msg
    notificationLogger tc loc src lvl' msg
  where minLvl = if verbose then LevelDebug else LevelInfo
