{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (storeMedia, loadMediaIDs, loadMedia, loadThumbnail,
                 MediaRow(..), row_media, row_thumbnail) where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO (..))
import qualified Data.ByteString.Lazy           as BL
import           Data.Coerce                    (coerce)
import           Database.SQLite.Simple         hiding (bind, close)
import           Database.SQLite.Simple.ToField

import           GoPro                          (Media (..))

createMediaStatement :: Query
createMediaStatement = "create table if not exists media (media_id primary key, captured_at, content_title, content_type, created_at, file_size, gopro_user_id, moments_count, on_public_profile, play_as, ready_to_edit, ready_to_view, resolution, source_duration, media_type, thumbnail)"

insertMediaStatement :: Query
insertMediaStatement = "insert into media (media_id, captured_at, content_title, content_type, created_at, file_size, gopro_user_id, moments_count, on_public_profile, play_as, ready_to_edit, ready_to_view, resolution, source_duration, media_type,thumbnail) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

data MediaRow = MediaRow {
  _row_media     :: Media,
  _row_thumbnail :: BL.ByteString
  }

makeLenses ''MediaRow

instance ToRow MediaRow where
  toRow (MediaRow Media{..} thumbnail) = [
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
    toField _media_type,
    toField thumbnail
    ]

storeMedia :: MonadIO m => FilePath -> [MediaRow] -> m ()
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


selectMediaStatement :: Query
selectMediaStatement = "select media_id, captured_at, content_title, content_type, created_at, file_size, gopro_user_id, moments_count, on_public_profile, play_as, ready_to_edit, ready_to_view, resolution, source_duration, media_type from media"

instance FromRow Media where
  fromRow =
    Media <$> field -- _media_id
    <*> field -- _captured_at
    <*> field -- _content_title
    <*> field -- _content_type
    <*> field -- _created_at
    <*> field -- _file_size
    <*> field -- _gopro_user_id
    <*> field -- _moments_count
    <*> field -- _on_public_profile
    <*> field -- _play_as
    <*> field -- _ready_to_edit
    <*> field -- _ready_to_view
    <*> field -- _resolution
    <*> field -- _source_duration
    <*> field -- _media_type
    <*> pure ""

loadMedia :: MonadIO m => FilePath -> m [Media]
loadMedia dbPath = liftIO $ withConnection dbPath sel
  where
    sel :: Connection -> IO [Media]
    sel db = query_ db selectMediaStatement

loadThumbnail :: MonadIO m => FilePath -> String -> m BL.ByteString
loadThumbnail dbPath imgid = liftIO $ withConnection dbPath sel
  where
    sel :: Connection -> IO BL.ByteString
    sel db = do
      [Only r] <- query db "select thumbnail from media where media_id = ?" (Only imgid) :: IO [Only BL.ByteString]
      pure r
