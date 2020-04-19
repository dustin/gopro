module GoPro.Logging where

import           Control.Monad         (when)
import           Control.Monad.Logger  (Loc (..), LogLevel (..), LogSource,
                                        LogStr, fromLogStr)
import qualified Data.ByteString.Char8 as C8
import           Data.String           (fromString)
import qualified Data.Text             as T
import           System.IO             (stderr)

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
