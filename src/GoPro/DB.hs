{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (storeMedia, loadMediaIDs) where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Coerce                    (coerce)
import           Database.SQLite.Simple         hiding (bind, close)
import           Database.SQLite.Simple.ToField

import           GoPro                          (Media (..))

createMediaStatement :: Query
createMediaStatement = "create table if not exists media (media_id primary key, captured_at, content_title, content_type, created_at, file_size, gopro_user_id, moments_count, on_public_profile, play_as, ready_to_edit, ready_to_view, resolution, source_duration, media_type)"

insertMediaStatement :: Query
insertMediaStatement = "insert into media (media_id, captured_at, content_title, content_type, created_at, file_size, gopro_user_id, moments_count, on_public_profile, play_as, ready_to_edit, ready_to_view, resolution, source_duration, media_type) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

instance ToRow Media where
  toRow Media{..} = [
    toField _media_id,
    toField _captured_at,
    toField _content_title,
    toField _content_type,
    toField _created_at,
    toField _file_size,
    toField _gopro_user_id,
    toField _moments_count,
    toField _on_public_profile,
    toField _play_as,
    toField _ready_to_edit,
    toField _ready_to_view,
    toField _resolution,
    toField _source_duration,
    toField _media_type
    ]

storeMedia :: MonadIO m => FilePath -> [Media] -> m ()
storeMedia dbPath media = liftIO $ withConnection dbPath up
  where up db = do
          execute_ db createMediaStatement
          executeMany db insertMediaStatement media

loadMediaIDs :: MonadIO m => FilePath -> m [String]
loadMediaIDs dbPath = coerce <$> (liftIO $ withConnection dbPath sel)
  where
    sel :: Connection -> IO [Only String]
    sel db = do
      execute_ db createMediaStatement
      query_ db "select media_id from media"
