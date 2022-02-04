{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GoPro.AuthDB (updateAuth, loadAuth) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple hiding (bind, close)

import           GoPro.Plus.Auth        (AuthInfo (..))

createStatement :: Query
createStatement = "create table if not exists authinfo (ts, owner_id, access_token, refresh_token, expires_in)"

insertStatement :: Query
insertStatement = "insert into authinfo(ts, owner_id, access_token, refresh_token, expires_in) values(current_timestamp, ?, ?, ?, ?)"

instance ToRow AuthInfo where
  toRow AuthInfo{..} = [toField _resource_owner_id, toField _access_token, toField _refresh_token, toField _expires_in]

instance FromRow AuthInfo where
  fromRow = AuthInfo <$> field <*> field <*> field <*> field

updateAuth :: MonadIO m => Connection -> AuthInfo -> m ()
updateAuth db ai = liftIO up
  where up = do
          execute_ db createStatement
          withTransaction db $ do
            execute_ db "delete from authinfo"
            execute db insertStatement ai

loadAuth :: MonadIO m => Connection -> m AuthInfo
loadAuth db = liftIO (head <$> query_ db "select access_token, expires_in, refresh_token, owner_id from authinfo")
