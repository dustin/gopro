{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB.Postgres (withPostgres) where

import           Control.Applicative                  (liftA2)
import           Control.Monad                        (void)
import           Control.Monad.Catch                  (bracket)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Aeson                           (ToJSON (..))
import qualified Data.Aeson                           as J
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Coerce                          (coerce)
import           Data.Foldable                        (traverse_)
import           Data.List                            (sortOn)
import           Data.List.Extra                      (groupOn)
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe, listToMaybe)
import           Data.String                          (fromString)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as TE
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (Binary)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Text.Read                            (readMaybe)

import           GoPro.DB
import           GoPro.Plus.Auth                      (AuthInfo (..))
import           GoPro.Plus.Media                     (Medium (..), MediumID, MediumType (..), Moment (..),
                                                       ReadyToViewType (..))
import           GoPro.Plus.Upload                    (DerivativeID, Upload (..), UploadPart (..))
import           GoPro.Resolve                        (MDSummary (..))

withPostgres :: String -> (Database -> IO a) -> IO a
withPostgres (fromString -> c) a = bracket (connectPostgreSQL c) close (a . mkDatabase)
  where
    mkDatabase db = Database {
      initTables = GoPro.DB.Postgres.initTables db,
      loadConfig = GoPro.DB.Postgres.loadConfig db,
      updateConfig = \m -> GoPro.DB.Postgres.updateConfig m db,
      updateAuth = GoPro.DB.Postgres.updateAuth db,
      loadAuth = GoPro.DB.Postgres.loadAuth db,
      storeMedia = \r -> GoPro.DB.Postgres.storeMedia r db,
      loadMediaIDs = GoPro.DB.Postgres.loadMediaIDs db,
      loadMediaRows = GoPro.DB.Postgres.loadMediaRows db,
      loadMedia = GoPro.DB.Postgres.loadMedia db,
      loadMedium = GoPro.DB.Postgres.loadMedium db,
      loadThumbnail = GoPro.DB.Postgres.loadThumbnail db,
      storeMoments = \mid moms -> GoPro.DB.Postgres.storeMoments mid moms db,
      loadMoments = GoPro.DB.Postgres.loadMoments db,
      momentsTODO = GoPro.DB.Postgres.momentsTODO db,
      metaBlobTODO = GoPro.DB.Postgres.metaBlobTODO db,
      insertMetaBlob = \m mdt bs -> GoPro.DB.Postgres.insertMetaBlob m mdt bs db,
      loadMetaBlob = GoPro.DB.Postgres.loadMetaBlob db,
      selectMetaBlob = GoPro.DB.Postgres.selectMetaBlob db,
      clearMetaBlob = \ms -> GoPro.DB.Postgres.clearMetaBlob ms db,
      metaTODO = GoPro.DB.Postgres.metaTODO db,
      insertMeta = \mid mds -> GoPro.DB.Postgres.insertMeta mid mds db,
      selectMeta = GoPro.DB.Postgres.selectMeta db,
      loadMeta = GoPro.DB.Postgres.loadMeta db,
      storeUpload = \fp mid up did part size -> GoPro.DB.Postgres.storeUpload fp mid up did part size db,
      completedUploadPart = \mid i p -> GoPro.DB.Postgres.completedUploadPart mid i p db,
      completedUpload = \mid i -> GoPro.DB.Postgres.completedUpload mid i db,
      listPartialUploads = GoPro.DB.Postgres.listPartialUploads db,
      clearUploads = GoPro.DB.Postgres.clearUploads db,
      listQueuedFiles = GoPro.DB.Postgres.listQueuedFiles db,
      listToCopyToS3 = GoPro.DB.Postgres.listToCopyToS3 db,
      queuedCopyToS3 = \stuff -> GoPro.DB.Postgres.queuedCopyToS3 stuff db,
      markS3CopyComplete = \stuff -> GoPro.DB.Postgres.markS3CopyComplete stuff db,
      listS3Waiting = GoPro.DB.Postgres.listS3Waiting db,
      listToCopyLocally = GoPro.DB.Postgres.listToCopyLocally db,
      selectAreas = GoPro.DB.Postgres.selectAreas db
      }

initQueries :: [(Int, Query)]
initQueries = [
  (1, [sql|create table if not exists media (media_id varchar primary key not null,
                                             camera_model varchar,
                                             captured_at timestamptz, created_at timestamptz,
                                             file_size int,
                                             moments_count int,
                                             source_duration varchar, -- numericish?
                                             media_type varchar, -- should be an enum
                                             width int, height int,
                                             ready_to_view varchar, -- should be enum
                                             thumbnail bytea,
                                             variants varchar, -- json,
                                             raw_json varchar, -- json,
                                             filename varchar
                                             )|]),
  (1, [sql|create table if not exists
           meta (media_id varchar primary key not null,
                 camera_model varchar,
                 captured_at timestamptz,
                 lat real, lon real,
                 max_speed_2d real, max_speed_3d real, max_faces int,
                 main_scene varchar,
                 main_scene_prob real,
                 max_distance real,
                 total_distance real)|]),
  (1, [sql|create table if not exists
           metablob (media_id varchar primary key, meta bytea, meta_length int, format varchar, backedup boolean)
           |]),
  (1, [sql|create table if not exists areas (area_id serial primary key,
                                             name text,
                                             lat1 real, lon1 real,
                                             lat2 real, lon2 real)|]),
  (1, "create table if not exists moments (media_id varchar, moment_id integer, timestamp integer)"),
  (1, "create index if not exists moments_by_medium on moments(media_id)"),
  (1, "create table if not exists config (key varchar, value varchar)"),
  (1, "insert into config values ('bucket', '')"),
  (1, "insert into config values ('s3copyfunc', 'download-to-s3')"),
  (1, "insert into config values ('s3copySQSQueue', '')"),
  (1, "create table if not exists uploads (filename varchar, media_id varchar, upid varchar, did varchar, partnum int, chunk_size int)"),
  (1, "create table if not exists upload_parts (media_id varchar, part int, partnum int)"),
  (1, "create table if not exists s3backup (media_id varchar, filename varchar, status varchar, response varchar)"),
  (1, "create unique index if not exists s3backup_by_file on s3backup(filename)"),
  (1, "create table if not exists authinfo (ts timestamptz, owner_id varchar, access_token varchar, refresh_token varchar, expires_in int)")
  ]

initTables :: MonadIO m => Connection -> m ()
initTables db = liftIO $ do
  [Only uv] <- query_ db "select coalesce(current_setting('gopro.version', true)::int, 0) as version"
  [Only dbname] <- query_ db "select current_database()"
  mapM_ (execute_ db) [q | (v,q) <- initQueries, v > uv]
  -- binding doesn't work on this for some reason.  It's safe, at least.
  void . execute_ db $ "set gopro.version to " <> (fromString . show . maximum . fmap fst $ initQueries)
  void . execute_ db $ "alter database " <> fromString dbname <> " set gopro.version from current"

-- A simple query.
q_ :: (MonadIO m, FromRow r) => Query -> Connection -> m [r]
q_ q = liftIO . flip query_ q

q' :: (MonadIO m, ToRow p, FromRow r) => Query -> p -> Connection -> m [r]
q' qq p db = liftIO $ query db qq p

-- A query that returns only a single column.
oq_ :: (MonadIO m, FromField r) => Query -> Connection -> m [r]
oq_ q db = unonly <$> q_ q db
  where
    unonly :: [Only a] -> [a]
    unonly = coerce

-- execute many
em :: (MonadIO m, ToRow r) => Query -> [r] -> Connection -> m ()
em q rs db = liftIO . void $ executeMany db q rs

-- execute
ex :: (MonadIO m, ToRow r) => Query -> r -> Connection -> m ()
ex q r db = liftIO . void $ execute db q r

-- execute_
ex_ :: MonadIO m => Query -> Connection -> m ()
ex_ q db = liftIO . void $ execute_ db q

loadConfig :: MonadIO m => Connection -> m (Map ConfigOption Text)
loadConfig db = Map.fromList <$> liftIO (query_ db "select key, value from config")

updateConfig :: MonadIO m => Map ConfigOption Text -> Connection -> m ()
updateConfig cfg db = ex_ "delete from config" db *> em "insert into config (key, value) values (?,?)" (Map.assocs cfg) db

upsertMediaStatement :: Query
upsertMediaStatement = [sql|insert into media (media_id, camera_model, captured_at, created_at,
                                               file_size, moments_count, source_duration, media_type,
                                               width, height, ready_to_view, filename, thumbnail, variants, raw_json)
                                        values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                              on conflict (media_id)
                                 do update
                                   set moments_count = excluded.moments_count,
                                       ready_to_view = excluded.ready_to_view,
                                       variants = excluded.variants,
                                       raw_json = excluded.raw_json
                              |]

instance FromField ConfigOption where
  fromField f bs = maybe (returnError ConversionFailed f "invalid value") pure $ strOption . TE.decodeUtf8 . fromMaybe "" $ bs

instance ToField ConfigOption where toField = toField . optionStr

instance ToField ReadyToViewType where
  toField = toField . show

instance FromField ReadyToViewType where
  fromField f = maybe (returnError ConversionFailed f "invalid value") pure . readMaybe . fromMaybe "" . fmap B8.unpack

instance ToField MediumType where
  toField = toField . show

instance FromField MediumType where
  fromField f = maybe (returnError ConversionFailed f "invalid value") pure . readMaybe . fromMaybe "" . fmap B8.unpack

instance ToRow MediaRow where
  toRow (MediaRow Medium{..} thumbnail vars raw) = [
    toField _medium_id,
    toField _medium_camera_model,
    toField _medium_captured_at,
    toField _medium_created_at,
    toField _medium_file_size,
    toField _medium_moments_count,
    toField _medium_source_duration,
    toField _medium_type,
    toField _medium_width,
    toField _medium_height,
    toField _medium_ready_to_view,
    toField _medium_filename,
    toField (Binary <$> thumbnail),
    toField vars,
    toField raw
    ]

storeMedia :: MonadIO m => [MediaRow] -> Connection -> m ()
storeMedia = em upsertMediaStatement

instance FromRow MediaRow where
  fromRow = MediaRow <$> fromRow <*> field <*> field <*> field

loadMediaRows :: MonadIO m => Connection -> m [MediaRow]
loadMediaRows = q_ "select media_id, camera_model, captured_at, created_at, file_size, moments_count, ready_to_view, source_duration, media_type, width, height, filename, thumbnail, variants, raw_json from media"

loadMediaIDs :: MonadIO m => Connection -> m [MediumID]
loadMediaIDs = oq_ "select media_id from media order by captured_at desc"

selectMediaStatement :: Query
selectMediaStatement = [sql|select media_id,
                                   camera_model,
                                   captured_at,
                                   created_at,
                                   file_size,
                                   moments_count,
                                   ready_to_view,
                                   source_duration,
                                   media_type,
                                   width,
                                   height,
                                   filename
                               from media
                               order by captured_at desc|]

selectMediumStatement :: Query
selectMediumStatement = [sql|select media_id,
                                    camera_model,
                                    captured_at,
                                    created_at,
                                    file_size,
                                    moments_count,
                                    ready_to_view,
                                    source_duration,
                                    media_type,
                                    width,
                                    height,
                                    filename
                                from media
                                where media_id = ?
                                order by captured_at desc|]

instance FromRow Medium where
  fromRow =
    Medium <$> field -- medium_id
    <*> field -- _medium_camera_model
    <*> field -- _medium_captured_at
    <*> field -- _medium_created_at
    <*> field -- _medium_file_size
    <*> field -- _medium_moments_count
    <*> field -- _medium_ready_to_view
    <*> field -- _medium_source_duration
    <*> field -- _medium_type
    <*> pure "" -- _medium_token
    <*> field -- _medium_width
    <*> field -- _medium_height
    <*> field -- filename

loadMedia :: MonadIO m => Connection -> m [Medium]
loadMedia = q_ selectMediaStatement

loadMedium :: MonadIO m => Connection -> MediumID -> m (Maybe Medium)
loadMedium db mid = listToMaybe <$> q' selectMediumStatement (Only mid) db

loadThumbnail :: MonadIO m => Connection -> MediumID -> m (Maybe BL.ByteString)
loadThumbnail db imgid = liftIO $ do
  [Only t] <- query db "select thumbnail from media where media_id = ?" (Only imgid)
  pure t

metaBlobTODO :: MonadIO m => Connection -> m [(MediumID, String)]
metaBlobTODO = q_ [sql|select media_id, media_type
                         from media
                         where media_id not in (select media_id from metablob)
                         order by created_at desc|]



instance FromField MetadataType where
  fromField f bs
    | bs == Nothing = returnError UnexpectedNull f ""
    | bs == Just "gpmf" = pure GPMF
    | bs == Just "exif" = pure EXIF
    | bs == Just "" = pure NoMetadata
    | otherwise     = returnError ConversionFailed f ""


instance ToField MetadataType where
  toField GPMF       = toField @Text "gpmf"
  toField EXIF       = toField @Text "exif"
  toField NoMetadata = toField @Text ""

insertMetaBlob :: MonadIO m => MediumID -> MetadataType -> Maybe BS.ByteString -> Connection -> m ()
insertMetaBlob mid fmt blob = ex "insert into metablob (media_id, meta, format, meta_length) values (?, ?, ?, ?)" (
          mid, Binary <$> blob, fmt, maybe 0 BS.length blob)

metaTODO :: MonadIO m => Connection -> m [(MediumID, MetadataType, BS.ByteString)]
metaTODO = q_ [sql|
                    select b.media_id, b.format, b.meta
                    from metablob b join media m on (m.media_id = b.media_id)
                    where b.meta is not null
                          and b.media_id not in (select media_id from meta)
                    |]

selectMetaBlob :: MonadIO m => Connection -> m [(MediumID, Maybe BS.ByteString)]
selectMetaBlob = q_ "select media_id, meta from metablob where meta is not null"

loadMetaBlob :: MonadIO m => Connection -> MediumID -> m (Maybe (MetadataType, Maybe BS.ByteString))
loadMetaBlob db mid = listToMaybe <$> q' "select format, meta from metablob where media_id = ?" (Only mid) db

clearMetaBlob :: MonadIO m => [MediumID] -> Connection -> m ()
clearMetaBlob ms db = traverse_ (\i -> ex "update metablob set meta = null, backedup = true where media_id = ?" (Only i) db) ms

insertMeta :: MonadIO m => MediumID -> MDSummary -> Connection -> m ()
insertMeta mid MDSummary{..} = liftIO . up
  where
    q = [sql|
          insert into meta (media_id, camera_model, captured_at, lat, lon,
                            max_speed_2d, max_speed_3d, max_faces,
                            main_scene, main_scene_prob, max_distance, total_distance)
                      values (?, ?, ?, ?, ?,
                              ?, ?, ?,
                              ?, ?, ?, ?)
          |]
    up db = void $ execute db q (
      mid, _cameraModel, _capturedTime, _lat, _lon,
      _maxSpeed2d, _maxSpeed3d, _maxFaces,
      show . fst <$> _mainScene, snd <$> _mainScene,
      _maxDistance, _totDistance)

instance FromRow Area where
  fromRow = Area <$> field <*> field <*> liftA2 (,) field field <*> liftA2 (,) field field

selectAreas :: MonadIO m => Connection -> m [Area]
selectAreas = q_ "select area_id, name, lat1, lon1, lat2, lon2 from areas"

storeMoments :: MonadIO m => MediumID -> [Moment] -> Connection -> m ()
storeMoments mid ms db = do
  ex "delete from moments where media_id = ?" (Only mid) db
  let vals = [(mid, _moment_id, _moment_time) | Moment{..} <- ms]
  em "insert into moments (media_id, moment_id, timestamp) values (?,?,?)" vals db

loadMoments :: MonadIO m => Connection -> m (Map MediumID [Moment])
loadMoments db = Map.fromListWith (<>) . map (\(a,b,c) -> (a,[Moment b c]))
              <$> q_ "select media_id, moment_id, timestamp from moments" db

momentsTODO :: MonadIO m => Connection -> m [MediumID]
momentsTODO = oq_ [sql|
                         select m.media_id from media m left outer join
                           (select media_id, count(*) as moco from moments group by media_id) as mo
                           on (m. media_id = mo.media_id)
                          where m.moments_count != ifnull(moco, 0) ;
                         |]

storeUpload :: MonadIO m => FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> Connection -> m ()
storeUpload fp mid Upload{..} did partnum chunkSize db = do
  ex "insert into uploads (filename, media_id, upid, did, partnum, chunk_size) values (?,?,?,?,?,?)" (
    fp, mid, _uploadID, did, partnum, chunkSize) db
  let vals = [(mid, _uploadPart, partnum) | UploadPart{..} <- _uploadParts]
  em "insert into upload_parts (media_id, part, partnum) values (?,?,?)" vals db

completedUploadPart :: MonadIO m => MediumID -> Integer -> Integer -> Connection -> m ()
completedUploadPart mid i p = ex "delete from upload_parts where media_id = ? and part = ? and partnum = ?" (mid, i, p)

completedUpload :: MonadIO m => MediumID -> Integer -> Connection -> m ()
completedUpload mid partnum = ex "delete from uploads where media_id = ? and partnum = ?" (mid, partnum)


instance FromRow PartialUpload where
  fromRow =
    PartialUpload <$> field -- fileName
    <*> field -- media_id
    <*> field -- upid
    <*> field -- did
    <*> field -- partnum
    <*> field -- chunkSize
    <*> pure []

-- Return in order of least work to do.
listPartialUploads :: MonadIO m => Connection -> m [[PartialUpload]]
listPartialUploads db = do
    segs <- Map.fromListWith (<>) . fmap (\(mid, p, pn) -> ((mid, pn), [p])) <$>
            q_ "select media_id, part, partnum from upload_parts" db
    sortOn (maximum . fmap (length . _pu_parts)) .
      groupOn _pu_medium_id .
      map (\p@PartialUpload{..} -> p{_pu_parts=Map.findWithDefault [] (_pu_medium_id, _pu_partnum) segs})
      <$> q_ "select filename, media_id, upid, did, partnum, chunk_size from uploads order by media_id" db

listQueuedFiles :: MonadIO m => Connection -> m [FilePath]
listQueuedFiles = oq_ "select filename from uploads"

listToCopyToS3 :: MonadIO m => Connection -> m [MediumID]
listToCopyToS3 = oq_ [sql|
                         select media_id from media
                         where media_id not in (select distinct media_id from s3backup)
                         order by created_at
                         |]

listS3Waiting :: MonadIO m => Connection -> m [String]
listS3Waiting = oq_ "select filename from s3backup where status is null"

queuedCopyToS3 :: MonadIO m => [(MediumID, String)] -> Connection -> m ()
queuedCopyToS3 = em "insert into s3backup (media_id, filename) values (?,?)"

markS3CopyComplete :: (MonadIO m, ToJSON j) => [(Text, Bool, j)] -> Connection -> m ()
markS3CopyComplete stuffs = em "update s3backup set status = ?, response = ? where filename = ?"
                            [(ok, J.encode res, fn) | (fn, ok, res) <- stuffs]

listToCopyLocally :: MonadIO m => Connection -> m [MediumID]
listToCopyLocally = oq_ "select media_id from media order by created_at"

clearUploads :: MonadIO m => Connection -> m ()
clearUploads db = ex_ "delete from upload_parts" db *> ex_ "delete from uploads" db

newtype NamedSummary = NamedSummary (MediumID, MDSummary)

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
    _maxDistance <- field
    _totDistance <- field
    pure $ NamedSummary (mid, MDSummary{..})

selectMeta :: forall m. MonadIO m => Connection -> m (Map MediumID MDSummary)
selectMeta db = Map.fromList . coerce <$> (q_ q db :: m [NamedSummary])
  where
    q = [sql|select media_id, camera_model, captured_at, lat, lon,
                    max_speed_2d, max_speed_3d,
                    max_faces, main_scene, main_scene_prob,
                    max_distance, total_distance
             from meta
             where camera_model is not null |]

loadMeta :: forall m. MonadIO m => Connection -> MediumID -> m (Maybe MDSummary)
loadMeta db m = fmap unName . listToMaybe <$> (q' q (Only m) db :: m [NamedSummary])
  where
    q = [sql|select media_id, camera_model, captured_at, lat, lon,
                    max_speed_2d, max_speed_3d,
                    max_faces, main_scene, main_scene_prob,
                    max_distance, total_distance
             from meta
             where media_id = ? |]
    unName (NamedSummary (_,b)) = b

-- Auth stuff

insertStatement :: Query
insertStatement = "insert into authinfo(ts, owner_id, access_token, refresh_token, expires_in) values(current_timestamp, ?, ?, ?, ?)"

authQuery :: Query
authQuery = "select access_token, expires_in, refresh_token, owner_id, ts - '30 minutes'::interval < current_timestamp as expired from authinfo"

instance ToRow AuthInfo where
  toRow AuthInfo{..} = [toField _resource_owner_id, toField _access_token, toField _refresh_token, toField _expires_in]

instance FromRow AuthInfo where
  fromRow = AuthInfo <$> field <*> field <*> field <*> field

updateAuth :: MonadIO m => Connection -> AuthInfo -> m ()
updateAuth db ai = liftIO up
  where up = withTransaction db $ do
               void $ execute_ db "delete from authinfo"
               void $ execute db insertStatement ai

loadAuth :: MonadIO m => Connection -> m AuthResult
loadAuth db = liftIO (head <$> query_ db authQuery)

instance FromRow AuthResult where
  fromRow = AuthResult <$> fromRow <*> field
