{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (storeMedia, loadMediaIDs, loadMedia, loadThumbnail,
                 MediaRow(..), row_media, row_thumbnail,
                 selectGPMFCandidates, insertGPMF, gpmfTODO, updateGPMF) where

import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO (..))
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Coerce                    (coerce)
import           Database.SQLite.Simple         hiding (bind, close)
import           Database.SQLite.Simple.ToField
import           Text.RawString.QQ              (r)

import           GoPro.Plus                     (Media (..))
import           GoPro.Resolve                  (MDSummary (..))

createMediaStatement :: Query
createMediaStatement = [r|create table if not exists media (media_id primary key, camera_model,
                                                            captured_at, created_at, file_size,
                                                            moments_count, source_duration,
                                                            media_type, width, height,
                                                            ready_to_view, thumbnail)|]

insertMediaStatement :: Query
insertMediaStatement = [r|insert into media (media_id, camera_model, captured_at, created_at,
                                             file_size, moments_count, source_duration, media_type,
                                             width, height, ready_to_view, thumbnail)
                                      values(?,?,?,?,?,?,?,?,?,?,?,?)|]

createGPMFStatement :: Query
createGPMFStatement = [r|create table if not exists
                               gpmf (media_id primary key, stream, camera_model, captured_at,
                                     lat, lon, max_speed_2d, max_speed_3d, max_faces, main_scene)|]

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
selectMediaStatement = [r|select media_id,
                                 camera_model,
                                 captured_at,
                                 created_at,
                                 file_size,
                                 moments_count,
                                 ready_to_view,
                                 source_duration,
                                 media_type,
                                 width,
                                 height
                             from media
                             order by captured_at desc|]

instance FromRow Media where
  fromRow =
    Media <$> field -- media_id
    <*> field -- _media_camera_model
    <*> field -- _media_captured_at
    <*> field -- _media_created_at
    <*> field -- _media_file_size
    <*> field -- _media_moments_count
    <*> field -- _media_ready_to_view
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
      [Only t] <- query db "select thumbnail from media where media_id = ?" (Only imgid)
      pure t

selectGPMFCandidates :: MonadIO m => Connection -> m [String]
selectGPMFCandidates db = coerce <$> liftIO sel
  where
    sel :: IO [Only String]
    sel = execute_ db createGPMFStatement >>
      query_ db [r|select media_id
                         from media
                         where media_id not in (select media_id from gpmf)
                             and media_type in ('Video', 'TimeLapseVideo')
                         order by created_at desc|]

insertGPMF :: MonadIO m => Connection -> String -> Maybe BS.ByteString -> m ()
insertGPMF db mid gpmf = liftIO ins
  where ins = execute db "insert into gpmf (media_id, stream) values (?, ?)" (mid, gpmf)

gpmfTODO :: MonadIO m => Connection -> m [(String, BS.ByteString)]
gpmfTODO db = liftIO sel
  where
    sel = query_ db "select media_id, stream from gpmf where stream is not null and camera_model is null"

updateGPMF :: MonadIO m => Connection -> String -> MDSummary -> m ()
updateGPMF db mid MDSummary{..} = liftIO up
  where
    up = executeNamed db [r|update gpmf set camera_model = :cam, captured_at = :ts, lat = :lat, lon = :lon,
                                        max_speed_2d = :speed2, max_speed_3d = :speed3, max_faces = :maxface,
                                        main_scene = :scene, main_scene_prob = :scene_prob
                                   where media_id = :mid|]
      [":cam" := cameraModel,
       ":ts" := capturedTime,
       ":lat" := lat,
       ":lon" := lon,
       ":speed2" := maxSpeed2d,
       ":speed3" := maxSpeed3d,
       ":maxface" := maxFaces,
       ":scene" := (show . fst <$> mainScene),
       ":scene_prob" := (snd <$> mainScene),
       ":mid" := mid]
