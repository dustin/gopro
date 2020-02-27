{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GoPro.AuthDB (updateAuth, loadAuth, loadToken) where

import           Control.Monad          (guard)
import           Control.Monad.IO.Class (MonadIO (..))
import           Database.SQLite.Simple hiding (bind, close)

import           GoPro                  (AuthResponse (..))

createStatement :: Query
createStatement = "create table if not exists authinfo (ts, access_token, refresh_token, expires_in)"

insertStatement :: Query
insertStatement = "insert into authinfo(ts, access_token, refresh_token, expires_in) values(current_timestamp, ?, ?, ?)"

deleteStatement :: Query
deleteStatement = "delete from authinfo"

selectStatement :: Query
selectStatement = "select access_token, refresh_token, expires_in from authinfo"

updateAuth :: MonadIO m => FilePath -> AuthResponse -> m ()
updateAuth dbPath AuthResponse{..} = liftIO $ withConnection dbPath up
  where up db = do
          execute_ db createStatement
          withTransaction db $ do
            execute_ db deleteStatement
            execute db insertStatement (_access_token, _refresh_token, _expires_in)

loadAuth :: MonadIO m => FilePath -> m AuthResponse
loadAuth dbPath = liftIO $ withConnection dbPath up
  where up db = do
          rows <- query_ db selectStatement :: IO [(String, String, Int)]
          guard (length rows == 1)
          let [(_access_token, _refresh_token, _expires_in)] = rows
          pure $ AuthResponse{..}

loadToken :: MonadIO m => FilePath -> m String
loadToken p = _access_token <$> loadAuth p
