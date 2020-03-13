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

import           GoPro.Plus                     (Media (..))

createMediaStatement :: Query
createMediaStatement = "create table if not exists media (media_id primary key, camera_model, captured_at, created_at, file_size, moments_count, resolution, source_duration, media_type, width, height, ready_to_view, thumbnail)"

insertMediaStatement :: Query
insertMediaStatement = "insert into media (media_id, camera_model, captured_at, created_at, file_size, moments_count, resolution, source_duration, media_type, width, height, ready_to_view, thumbnail) values(?,?,?,?,?,?,?,?,?,?,?,?,?)"

data MediaRow = MediaRow {
  _row_media     :: Media,
  _row_thumbnail :: BL.ByteString
  }

makeLenses ''MediaRow

instance ToRow MediaRow where
  toRow (MediaRow Media{..} thumbnail) = [
    toField _media_id,
    toField _media_camera_model,
    toField _media_captured_at,
    toField _media_created_at,
    toField _media_file_size,
    toField _media_moments_count,
    toField _media_resolution,
    toField _media_source_duration,
    toField _media_type,
    toField _media_width,
    toField _media_height,
    toField _media_ready_to_view,
    toField thumbnail
    ]

storeMedia :: MonadIO m => Connection -> [MediaRow] -> m ()
storeMedia db media = liftIO up
  where up = do
          execute_ db createMediaStatement
          executeMany db insertMediaStatement media

loadMediaIDs :: MonadIO m => Connection -> m [String]
loadMediaIDs db = coerce <$> liftIO sel
  where
    sel :: IO [Only String]
    sel = do
      execute_ db createMediaStatement
      query_ db "select media_id from media order by captured_at desc"


selectMediaStatement :: Query
selectMediaStatement = "select media_id, camera_model, captured_at, created_at, file_size, moments_count, ready_to_view, resolution, source_duration, media_type, width, height from media order by captured_at desc"

instance FromRow Media where
  fromRow =
    Media <$> field -- media_id
    <*> field -- _media_camera_model
    <*> field -- _media_captured_at
    <*> field -- _media_created_at
    <*> field -- _media_file_size
    <*> field -- _media_moments_count
    <*> field -- _media_ready_to_view
    <*> field -- _media_resolution
    <*> field -- _media_source_duration
    <*> field -- _media_type
    <*> pure "" -- _media_token
    <*> field -- _media_width
    <*> field -- _media_height

loadMedia :: MonadIO m => Connection -> m [Media]
loadMedia db = liftIO $ query_ db selectMediaStatement

loadThumbnail :: MonadIO m => Connection -> String -> m BL.ByteString
loadThumbnail db imgid = liftIO sel
  where
    sel :: IO BL.ByteString
    sel = do
      [Only r] <- query db "select thumbnail from media where media_id = ?" (Only imgid) :: IO [Only BL.ByteString]
      pure r
