module GoPro.Logging where

import           Control.Concurrent.STM (TChan, atomically, newBroadcastTChanIO,
                                         writeTChan)
import           Control.Monad          (when)
import           Control.Monad.Logger   (Loc (..), LogLevel (..), LogSource,
                                         LogStr, fromLogStr)
import qualified Data.ByteString.Char8  as C8
import           Data.String            (fromString)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           System.IO              (stderr)

import           GoPro.Notification

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

notificationChanLogger :: TChan Notification -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
notificationChanLogger ch _ _ lvl str = case lvl of
                                          LevelDebug -> pure ()
                                          LevelInfo  -> note NotificationInfo
                                          _          -> note NotificationError
  where note t = atomically $ writeTChan ch (Notification 0 t "GoPro" lstr)
        lstr = TE.decodeUtf8 $ fromLogStr str

mkLogChannel :: IO (TChan Notification)
mkLogChannel = newBroadcastTChanIO
