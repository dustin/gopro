{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GoPro.AuthDB (AuthResult(..), updateAuth, loadAuth) where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Database.SQLite.Simple         hiding (bind, close)
import           Database.SQLite.Simple.ToField

import           GoPro.Plus.Auth                (AuthInfo (..))

createStatement :: Query
createStatement = "create table if not exists authinfo (ts, owner_id, access_token, refresh_token, expires_in)"

insertStatement :: Query
insertStatement = "insert into authinfo(ts, owner_id, access_token, refresh_token, expires_in) values(current_timestamp, ?, ?, ?, ?)"

authQuery :: Query
authQuery = "select access_token, expires_in, refresh_token, owner_id, (datetime(ts, '-30 minutes', '+' || cast(expires_in as text) || ' seconds')) < current_timestamp as expired from authinfo"

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

data AuthResult = AuthResult {
  arInfo    :: AuthInfo,
  arExpired :: Bool
  }
  deriving Show

instance FromRow AuthResult where
  fromRow = AuthResult <$> fromRow <*> field

loadAuth :: MonadIO m => Connection -> m AuthResult
loadAuth db = liftIO (head <$> query_ db authQuery)
