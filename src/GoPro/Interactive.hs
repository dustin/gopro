module GoPro.Interactive where

import           GoPro.Commands
import GoPro.DB.Sqlite
import           Database.SQLite.Simple           hiding (bind, close)


-- | A convenience function for performing GoPro actions from within GHCI.
interactively :: String -> GoPro a -> IO a
interactively dbp a = withConnection dbp $ \db -> withSQLiteDB db $
  runWithOptions (Options dbp "static" True 3 11 (6*1024*1024) Nothing SyncCmd) a
