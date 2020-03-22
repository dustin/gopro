{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (storeMedia, loadMediaIDs, loadMedia, loadThumbnail,
                 MediaRow(..), row_media, row_thumbnail,
                 metaTODO, insertMetaBlob, gpmfTODO,
                 updateGPMF, selectGPMF, initTables) where

import           Control.Applicative            (liftA2)
import           Control.Lens
import           Control.Monad.IO.Class         (MonadIO (..))
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Coerce                    (coerce)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Database.SQLite.Simple         hiding (bind, close)
import           Database.SQLite.Simple.ToField
import           Text.RawString.QQ              (r)

import           GoPro.Plus                     (Media (..))
import           GoPro.Resolve                  (MDSummary (..))

initTables :: Connection -> IO ()
initTables db = mapM_ (execute_ db)
                [[r|create table if not exists media (media_id primary key, camera_model,
                                                      captured_at, created_at, file_size,
                                                      moments_count, source_duration,
                                                      media_type, width, height,
                                                      ready_to_view, thumbnail)|],
                 [r|create table if not exists
                           gpmf (media_id primary key, stream, camera_model, captured_at,
                                 lat, lon, max_speed_2d, max_speed_3d, max_faces, main_scene, main_scene_prob)|],
                  "create table if not exists metablob (media_id primary key, meta blob)"]

insertMediaStatement :: Query
insertMediaStatement = [r|insert into media (media_id, camera_model, captured_at, created_at,
                                             file_size, moments_count, source_duration, media_type,
                                             width, height, ready_to_view, thumbnail)
                                      values(?,?,?,?,?,?,?,?,?,?,?,?)|]

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
  where up = executeMany db insertMediaStatement media

loadMediaIDs :: MonadIO m => Connection -> m [String]
loadMediaIDs db = coerce <$> liftIO sel
  where
    sel :: IO [Only String]
    sel = query_ db "select media_id from media order by captured_at desc"


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
    <*> pure Nothing

loadMedia :: MonadIO m => Connection -> m [Media]
loadMedia db = liftIO $ query_ db selectMediaStatement

loadThumbnail :: MonadIO m => Connection -> String -> m BL.ByteString
loadThumbnail db imgid = liftIO sel
  where
    sel :: IO BL.ByteString
    sel = do
      [Only t] <- query db "select thumbnail from media where media_id = ?" (Only imgid)
      pure t

metaTODO :: MonadIO m => Connection -> m [(String, String)]
metaTODO db = liftIO sel
  where
    sel :: IO [(String, String)]
    sel = query_ db [r|select media_id, media_type
                       from media
                       where media_id not in (select media_id from metablob)
                       order by created_at desc|]

insertMetaBlob :: MonadIO m => Connection -> String -> Maybe BS.ByteString -> m ()
insertMetaBlob db mid blob = liftIO ins
  where ins = execute db "insert into metablob (media_id, meta) values (?, ?)" (mid, blob)

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
      [":cam" := _cameraModel,
       ":ts" := _capturedTime,
       ":lat" := _lat,
       ":lon" := _lon,
       ":speed2" := _maxSpeed2d,
       ":speed3" := _maxSpeed3d,
       ":maxface" := _maxFaces,
       ":scene" := (show . fst <$> _mainScene),
       ":scene_prob" := (snd <$> _mainScene),
       ":mid" := mid]

newtype NamedSummary = NamedSummary (String, MDSummary)

instance FromRow NamedSummary where
  fromRow = do
    mid <- field
    _cameraModel <- field
    _capturedTime <- field
    _lat <- field
    _lon <- field
    _maxSpeed2d <- field
    _maxSpeed3d <- field
    _maxFaces <- field
    loc <- fmap read <$> field
    prob <- field
    let _mainScene = liftA2 (,) loc prob
    pure $ NamedSummary (mid, MDSummary{..})

selectGPMF :: MonadIO m => Connection -> m (Map String MDSummary)
selectGPMF db = Map.fromList . coerce <$> liftIO sel
  where
    sel :: IO [NamedSummary]
    sel = query_ db [r|select media_id, camera_model, captured_at, lat, lon,
                              max_speed_2d, max_speed_3d,
                              max_faces, main_scene, main_scene_prob
                          from gpmf
                          where camera_model is not null |]
