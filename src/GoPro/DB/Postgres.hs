{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB.Postgres (withPostgres) where

import           Control.Applicative        (liftA2)
import           Control.Foldl              (Fold (..))
import           Control.Monad.Catch        (bracket)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Aeson                 (ToJSON (..))
import qualified Data.Aeson                 as J
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Foldable              (for_, toList, traverse_)
import           Data.Int                   (Int32, Int64)
import           Data.List                  (sortOn)
import           Data.List.Extra            (groupOn)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Ratio
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (DiffTime, UTCTime (..))
import           Hasql.TH
import           Prelude                    hiding (init)
import           Text.RawString.QQ
import           Text.Read                  (readMaybe)

import           Hasql.Connection           (Connection)
import qualified Hasql.Connection           as Connection
import           Hasql.Decoders             (Row, Value, column, foldlRows, noResult, nonNullable, nullable, rowList,
                                             rowMaybe, singleRow)
import qualified Hasql.Decoders             as Decoders
import           Hasql.Encoders             (noParams)
import qualified Hasql.Encoders             as Encoders
import qualified Hasql.Session              as Session
import           Hasql.Statement            (Statement (..))
import qualified Hasql.Transaction          as TX
import           Hasql.Transaction.Sessions (transaction)
import qualified Hasql.Transaction.Sessions as TX

import           GoPro.DB
import           GoPro.DEVC                 (GPSReading (..))
import           GoPro.Plus.Auth            (AuthInfo (..))
import           GoPro.Plus.Media           (Medium (..), MediumID, Moment (..))
import           GoPro.Plus.Upload          (DerivativeID, Upload (..), UploadPart (..))
import           GoPro.Resolve              (MDSummary (..))

withConn :: Connection.Settings -> (Connection -> IO a) -> IO a
withConn s = bracket (mightFail $ Connection.acquire s) Connection.release

withPostgres :: String -> (Database -> IO a) -> IO a
withPostgres (fromString -> c) a = withConn c (a . mkDatabase)
  where
    mkDatabase db = Database {
      initTables = GoPro.DB.Postgres.initTables db,
      loadConfig = GoPro.DB.Postgres.loadConfig db,
      updateConfig = \m -> GoPro.DB.Postgres.updateConfig m db,
      updateAuth = GoPro.DB.Postgres.updateAuth db,
      loadAuth = GoPro.DB.Postgres.loadAuth db,
      storeMedia = \m -> GoPro.DB.Postgres.storeMedia m db,
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
      selectAreas = GoPro.DB.Postgres.selectAreas db,
      fixupQuery = GoPro.DB.Postgres.fixupQuery db,
      loadGPSReadings = GoPro.DB.Postgres.loadGPSReadings db,
      storeGPSReadings = GoPro.DB.Postgres.storeGPSReadings db,
      gpsReadingsTODO = GoPro.DB.Postgres.gpsReadingsTODO db
      }

initQueries :: [(Int64, ByteString)]
initQueries = [
  (1, [r|create table if not exists media (media_id varchar primary key not null,
                                           camera_model varchar,
                                           captured_at timestamptz, created_at timestamptz,
                                           file_size int8,
                                           moments_count int,
                                           source_duration varchar, -- numericish?
                                           media_type varchar, -- should be an enum
                                           width int, height int,
                                           ready_to_view varchar, -- should be enum
                                           thumbnail bytea,
                                           variants bytea, -- json,
                                           raw_json bytea, -- json,
                                           filename varchar
                                           )|]),
  (1, [r|create table if not exists
         meta (media_id varchar primary key not null,
               camera_model varchar,
               captured_at timestamptz,
               lat float8, lon float8,
               max_speed_2d float8, max_speed_3d float8, max_faces int,
               main_scene varchar,
               main_scene_prob float4,
               max_distance float8,
               total_distance float8)|]),
  (1, [r|create table if not exists
         metablob (media_id varchar primary key, meta bytea, meta_length int, format varchar, backedup boolean)
         |]),
  (1, [r|create table if not exists areas (area_id serial primary key,
                                           name text,
                                           lat1 float8, lon1 float8,
                                           lat2 float8, lon2 float8)|]),
  (1, "create table if not exists moments (media_id varchar, moment_id varchar, timestamp int)"),
  (1, "create index if not exists moments_by_medium on moments(media_id)"),
  (1, "create table if not exists config (key varchar, value varchar)"),
  (1, "insert into config values ('bucket', '')"),
  (1, "insert into config values ('s3copyfunc', 'download-to-s3')"),
  (1, "insert into config values ('s3copySQSQueue', '')"),
  (1, "create table if not exists uploads (filename varchar, media_id varchar, upid varchar, did varchar, partnum int, chunk_size int)"),
  (1, "create table if not exists upload_parts (media_id varchar, part int, partnum int)"),
  (1, "create table if not exists s3backup (media_id varchar, filename varchar, status varchar, response varchar)"),
  (1, "create unique index if not exists s3backup_by_file on s3backup(filename)"),
  (1, "create table if not exists authinfo (ts timestamptz, owner_id varchar, access_token varchar, refresh_token varchar, expires_in int)"),

  -- Constraints
  (2, [r|alter table meta add constraint fk_meta_mid FOREIGN KEY (media_id) REFERENCES media (media_id);
         alter table metablob add constraint fk_metablob_mid FOREIGN KEY (media_id) REFERENCES media (media_id);
         alter table moments add constraint fk_moments_mid FOREIGN KEY (media_id) REFERENCES media (media_id);
         create unique index uploads_pk on uploads (media_id, partnum);
         alter table upload_parts add constraint fk_upload_parts FOREIGN KEY (media_id, partnum) REFERENCES uploads (media_id, partnum);
         alter table s3backup add constraint fk_s3backup_mid FOREIGN KEY (media_id) REFERENCES media (media_id);
        |]),

  -- Clean up source duration value
  (3, "alter table media alter column source_duration type interval using (source_duration || ' milliseconds')::interval"),

  -- Nullability constraints
  (4, "alter table media alter column created_at set not null"),
  (4, "alter table media alter column captured_at set not null"),
  (4, "alter table media alter column file_size set not null"),
  (4, "alter table media alter column moments_count set not null"),
  (4, "alter table media alter column media_type set not null"),
  (4, "alter table metablob alter column format set not null"),
  (4, "alter table metablob alter column meta_length set not null"),
  (4, "update metablob set backedup = false where backedup is null"),
  (4, "alter table metablob alter column backedup set not null"),
  (4, "alter table uploads alter column filename set not null"),
  (4, "alter table uploads alter column media_id set not null"),
  (4, "alter table uploads alter column upid set not null"),
  (4, "alter table uploads alter column did set not null"),
  (4, "alter table uploads alter column partnum set not null"),
  (4, "alter table uploads alter column chunk_size set not null"),
  (4, "alter table upload_parts alter column media_id set not null"),
  (4, "alter table upload_parts alter column part set not null"),
  (4, "alter table upload_parts alter column partnum set not null"),
  (4, "alter table moments alter column media_id set not null"),
  (4, "alter table moments alter column moment_id set not null"),

  -- We now populate metablob before we start uploading media
  (5, "alter table metablob drop constraint fk_metablob_mid"),

  (7, "drop table if exists gps_readings"),

  (8, [r|create table if not exists gps_readings (
        media_id varchar not null,
        timestamp timestamptz not null,
        lat float8 not null,
        lon float8 not null,
        altitude float8 not null,
        speed2d float8 not null,
        speed3d float8 not null,
        dop float8 not null,
        fix int4 not null)
    |]),

  (8, "create index gps_readings_by_media_id on gps_readings(media_id)")
  ]

initTables :: MonadIO m => Connection -> m ()
initTables = mightFail . Session.run sess
  where
    sess = do
      uv <- Session.statement () $ Statement "select coalesce(current_setting('gopro.version', true)::int, 0) as version"
            noParams (Decoders.singleRow (column (Decoders.nonNullable Decoders.int8))) True
      dbname <- Session.statement () $ Statement "select current_database()" noParams (Decoders.singleRow (column (nonNullable Decoders.bytea))) True
      mapM_ Session.sql [q | (v,q) <- initQueries, v > uv]
      Session.sql $ "set gopro.version to " <> (fromString . show . maximum . fmap fst $ initQueries)
      Session.sql $ "alter database " <> dbname <> " set gopro.version from current"

mightFail :: (MonadIO m, Show s) => IO (Either s a) -> m a
mightFail a = either (liftIO . fail . show) pure =<< liftIO a

loadConfig :: MonadIO m => Connection -> m (Map ConfigOption Text)
loadConfig = mightFail . Session.run (Session.statement () st)
  where
    st :: Statement () (Map ConfigOption Text)
    st = [foldStatement|select key :: text, value :: text from config|] (Fold f mempty conv)

    f :: Map Text Text -> (Text, Text) -> Map Text Text
    f m (k,v) = Map.insert k v m
    conv = Map.mapKeys (fromMaybe (error "invalid option in db") . strOption)

updateConfig :: MonadIO m => Map ConfigOption Text -> Connection -> m ()
updateConfig cfg db = mightFail $ flip Session.run db $ do
  Session.sql "delete from config"
  traverse_ (flip Session.statement ins . f) $ Map.assocs cfg

    where
      ins = [resultlessStatement|insert into config (key, value) values ($1::text, $2::text)|]
      f (k,v) = (optionStr k, v)

upsertMediaS :: Statement (Text, Maybe Text, UTCTime, UTCTime,
                           Maybe Int64, Int64, Maybe DiffTime, Text,
                           Maybe Int64, Maybe Int64, Text, Maybe Text, Maybe ByteString, ByteString, ByteString) ()
upsertMediaS = [resultlessStatement|insert into media (media_id, camera_model, captured_at, created_at,
                                     file_size, moments_count, source_duration, media_type,
                                     width, height, ready_to_view, filename, thumbnail, variants, raw_json)
                                     values($1 :: text, $2 :: text?, $3 :: timestamptz, $4 :: timestamptz,
                                            $5 :: int8?, $6 :: int8, $7 :: interval?, $8 :: text,
                                            $9 :: int8?, $10 :: int8?, $11 :: text, $12 :: text?, $13 :: bytea?, $14 :: bytea, $15 :: bytea)
                              on conflict (media_id)
                                 do update
                                   set moments_count = excluded.moments_count,
                                       ready_to_view = excluded.ready_to_view,
                                       variants = excluded.variants,
                                       raw_json = excluded.raw_json
                              |]

storeMedia :: MonadIO m => [MediaRow] -> Connection -> m ()
storeMedia rows = mightFail . Session.run sess
  where
    sess = mapM_ one rows
    one (MediaRow Medium{..} thumbnail vars raw) = Session.statement (
      _medium_id,
      T.pack <$> _medium_camera_model,
      _medium_captured_at,
      _medium_created_at,
      fromIntegral <$> _medium_file_size,
      fromIntegral _medium_moments_count,
      parseMS =<< _medium_source_duration,
      T.pack . show $ _medium_type,
      fromIntegral <$> _medium_width,
      fromIntegral <$> _medium_height,
      T.pack . show $ _medium_ready_to_view,
      T.pack <$> _medium_filename,
      BL.toStrict <$> thumbnail,
      BL.toStrict vars,
      BL.toStrict raw
      ) upsertMediaS

    parseMS :: String -> Maybe DiffTime
    parseMS s = fromRational . (% 1000) <$> readMaybe s

mediaRow :: Row MediaRow
mediaRow = MediaRow <$> mediumRow
             <*> column (Decoders.nullable (BL.fromStrict <$> Decoders.bytea))
             <*> column (Decoders.nonNullable (BL.fromStrict <$> Decoders.bytea))
             <*> column (Decoders.nonNullable (BL.fromStrict <$> Decoders.bytea))

loadMediaRows :: MonadIO m => Connection -> m [MediaRow]
loadMediaRows = mightFail . Session.run (Session.statement () st)
  where
    st = Statement "select media_id, camera_model, captured_at, created_at, file_size, moments_count, ready_to_view, extract('milliseconds' from source_duration)::text, media_type, width, height, filename, thumbnail, variants, raw_json from media" noParams (rowList mediaRow) True

queryStrings :: MonadIO m => ByteString -> Connection -> m [Text]
queryStrings q = mightFail . Session.run (Session.statement () l)
  where
    l = Statement q noParams (rowList ((column . nonNullable) Decoders.text)) True

loadMediaIDs :: MonadIO m => Connection -> m [MediumID]
loadMediaIDs = queryStrings "select media_id from media order by captured_at desc"

mediumRow :: Row Medium
mediumRow = Medium
            <$> (column . nonNullable) Decoders.text -- medium_id
            <*> (column . nullable) str -- medium_camera_model
            <*> (column . nonNullable) Decoders.timestamptz -- _medium_captured_at
            <*> (column . nonNullable) Decoders.timestamptz -- _medium_created_at
            <*> (column . nullable) int -- _medium_file_size
            <*> (column . nonNullable) int -- _medium_moments_count
            <*> (column . nonNullable) readr -- _medium_ready_to_view
            <*> (column . nullable) str -- _medium_source_duration
            <*> (column . nonNullable) readr -- _medium_type
            <*> pure "" -- _medium_token
            <*> (column . nullable) int -- _medium_width
            <*> (column . nullable) int -- _medium_height
            <*> (column . nullable) str -- filename
  where
    int = fromIntegral <$> Decoders.int8
    str = T.unpack <$> Decoders.text

readr :: Read r => Value r
readr = Decoders.enum (readMaybe . T.unpack)

loadMedia :: MonadIO m => Connection -> m [Medium]
loadMedia = mightFail . Session.run (Session.statement () st)
  where
    st = Statement sql noParams (rowList mediumRow) True
    sql = [r|select media_id,
                    camera_model,
                    captured_at,
                    created_at,
                    file_size,
                    moments_count,
                    ready_to_view,
                    extract('milliseconds' from source_duration)::text,
                    media_type,
                    width,
                    height,
                    filename
                 from media
                 order by captured_at desc|]

loadMedium :: MonadIO m => Connection -> MediumID -> m (Maybe Medium)
loadMedium db mid = mightFail . Session.run (Session.statement mid st) $ db
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) (rowMaybe mediumRow) True
    sql = [r|select media_id,
                    camera_model,
                    captured_at,
                    created_at,
                    file_size,
                    moments_count,
                    ready_to_view,
                    extract('milliseconds' from source_duration)::text,
                    media_type,
                    width,
                    height,
                    filename
                 from media
                 where media_id = $1|]

loadThumbnail :: MonadIO m => Connection -> MediumID -> m (Maybe BL.ByteString)
loadThumbnail db imgid = (fmap.fmap) BL.fromStrict . mightFail . Session.run (Session.statement imgid st) $ db
  where
    st = [maybeStatement|select thumbnail::bytea from media where media_id = $1::text|]

metaBlobTODO :: MonadIO m => Connection -> m [(MediumID, String)]
metaBlobTODO = fmap ((fmap.fmap) T.unpack . toList) . mightFail . Session.run (Session.statement () st)
  where
    st = [vectorStatement|select media_id::text, media_type::text
                                 from media
                                 where media_id not in (select media_id from metablob)
                                 order by created_at desc|]

tshow :: Show a => a -> Text
tshow = T.pack . show

insertMetaBlob :: MonadIO m => MediumID -> MetadataType -> Maybe BS.ByteString -> Connection -> m ()
insertMetaBlob mid fmt blob =
  mightFail . Session.run (Session.statement (mid, tshow fmt, blob, fromIntegral $ maybe 0 BS.length blob) st)
  where
    st = [resultlessStatement|
            insert into metablob (media_id, format, meta, meta_length, backedup) values ($1::text, $2::text, $3::bytea?, $4::int8, false)
         |]

metaTODO :: MonadIO m => Connection -> m [(MediumID, MetadataType, BS.ByteString)]
metaTODO = mightFail . Session.run (Session.statement () st)
  where
    st = Statement sql noParams (rowList dec) True
    dec = (,,) <$> column (nonNullable Decoders.text) <*> column (nonNullable readr) <*> column (nonNullable Decoders.bytea)
    sql = [r|select b.media_id, b.format, b.meta
                    from metablob b join media m on (m.media_id = b.media_id)
                    where b.meta is not null
                          and b.media_id not in (select media_id from meta)
            |]

selectMetaBlob :: MonadIO m => Connection -> m [(MediumID, Maybe BS.ByteString)]
selectMetaBlob = mightFail . Session.run (Session.statement () st)
  where
    st = Statement sql noParams (rowList dec) True
    dec = (,) <$> column (nonNullable Decoders.text) <*> column (nullable Decoders.bytea)
    sql = "select media_id, meta from metablob where meta is not null"

loadMetaBlob :: MonadIO m => Connection -> MediumID -> m (Maybe (MetadataType, Maybe BS.ByteString))
loadMetaBlob db mid = fmap resolve . mightFail . Session.run (Session.statement mid st) $ db
  where
    resolve :: Maybe (Text, Maybe ByteString) -> Maybe (MetadataType, Maybe ByteString)
    resolve Nothing       = Nothing
    resolve (Just (t, b)) = maybe Nothing (\x -> Just (x, b)) . readMaybe . T.unpack $ t
    st = [maybeStatement|select format::text, meta::bytea? from metablob where media_id = $1::text|]

clearMetaBlob :: MonadIO m => [MediumID] -> Connection -> m ()
clearMetaBlob ms = mightFail . Session.run (traverse_ (\i -> Session.statement i st) ms)
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) noResult True
    sql = "update metablob set meta = null, backedup = true where media_id = $1"

insertMeta :: MonadIO m => MediumID -> MDSummary -> Connection -> m ()
insertMeta mid MDSummary{..} = mightFail . Session.run (Session.statement args ins)
  where
    args = (mid, T.pack _cameraModel, _capturedTime, _lat, _lon,
            _maxSpeed2d, _maxSpeed3d, _maxDistance, _totDistance,
            fromIntegral _maxFaces, tshow . fst <$> _mainScene, snd <$> _mainScene)


    ins :: Statement (Text, Text, Maybe UTCTime, Maybe Double, Maybe Double,
                      Maybe Double, Maybe Double, Maybe Double, Maybe Double,
                      Int32, Maybe Text, Maybe Float) ()
    ins = [resultlessStatement|
          insert into meta (media_id, camera_model, captured_at, lat, lon,
                            max_speed_2d, max_speed_3d, max_distance, total_distance,
                            max_faces, main_scene, main_scene_prob)
                      values ($1::text, $2::text, $3::timestamptz?, $4::float8?, $5::float8?,
                              $6::float8?, $7::float8?, $8::float8?, $9::float8?,
                              $10::int4, $11::text?, $12::float4?)
                              |]

areaRow :: Row Area
areaRow = Area <$> (fromIntegral <$> (column . nonNullable) Decoders.int8)
               <*> (T.unpack <$> (column . nonNullable) Decoders.text)
               <*> liftA2 (,) f f
               <*> liftA2 (,) f f
  where
    f = (column . nonNullable) Decoders.float8

selectAreas :: MonadIO m => Connection -> m [Area]
selectAreas = mightFail . Session.run (Session.statement () st)
  where
    st = Statement "select area_id, name, lat1, lon1, lat2, lon2 from areas" noParams (rowList areaRow) True

storeMoments :: MonadIO m => MediumID -> [Moment] -> Connection -> m ()
storeMoments mid ms = mightFail . Session.run (transaction TX.Serializable TX.Write tx)
  where
    tx = do
      TX.statement mid del
      traverse_ (`TX.statement` ins) [(mid, _moment_id, fromIntegral <$> _moment_time) | Moment{..} <- ms]

    del = [resultlessStatement|delete from moments where media_id = $1::text|]
    ins = [resultlessStatement|insert into moments (media_id, moment_id, timestamp) values ($1::text,$2::text,$3::int?)|]

loadMoments :: MonadIO m => Connection -> m (Map MediumID [Moment])
loadMoments = fmap (Map.fromListWith (<>) . foldMap f) . mightFail . Session.run (Session.statement () st)
  where
    st = [vectorStatement|select media_id::text, moment_id::text, timestamp::int? from moments|]
    f (m,i,t) = [(m, [Moment i (fromIntegral <$> t)])]

momentsTODO :: MonadIO m => Connection -> m [MediumID]
momentsTODO = queryStrings sql
  where
    sql = [r|
            select m.media_id from media m left outer join
                   (select media_id, count(*) as moco from moments group by media_id) as mo
                           on (m.media_id = mo.media_id)
                        where m.moments_count != coalesce(moco, 0)
            |]

storeUpload :: MonadIO m => FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> Connection -> m ()
storeUpload fp mid Upload{..} did partnum chunkSize db = mightFail . flip Session.run db $ do
  Session.statement (T.pack fp, mid, _uploadID, did, fromIntegral partnum, fromIntegral chunkSize) upSt
  for_ _uploadParts $ \UploadPart{..} -> Session.statement (mid, fromIntegral _uploadPart, fromIntegral partnum) partSt
  where
    upSt = [resultlessStatement|
             insert into uploads (filename, media_id, upid, did, partnum, chunk_size) values ($1::text, $2::text, $3::text, $4::text, $5::int, $6::int)|]
    partSt = [resultlessStatement|insert into upload_parts (media_id, part, partnum) values ($1::text, $2::int, $3::int)|]

completedUploadPart :: MonadIO m => MediumID -> Integer -> Integer -> Connection -> m ()
completedUploadPart mid i p = mightFail . Session.run (Session.statement (mid, fromIntegral i, fromIntegral p) st)
  where
    st :: Statement (Text, Int32, Int32) ()
    st = [resultlessStatement|delete from upload_parts where media_id = $1::text and part = $2::int and partnum = $3::int|]

completedUpload :: MonadIO m => MediumID -> Integer -> Connection -> m ()
completedUpload mid p = mightFail . Session.run (Session.statement (mid, fromIntegral p) st)
  where
    st :: Statement (Text, Int32) ()
    st = [resultlessStatement|delete from uploads where media_id = $1::text and partnum = $2::int|]

partialUploadRow :: Row PartialUpload
partialUploadRow = PartialUpload
                   <$> (column . nonNullable) str
                   <*> (column . nonNullable) Decoders.text
                   <*> (column . nonNullable) Decoders.text
                   <*> (column . nonNullable) Decoders.text
                   <*> (column . nonNullable) int
                   <*> (column . nonNullable) int
                   <*> pure []
  where
    int = fromIntegral <$> Decoders.int8
    str = T.unpack <$> Decoders.text

-- Return in order of least work to do.
listPartialUploads :: MonadIO m => Connection -> m [[PartialUpload]]
listPartialUploads db = mightFail . flip Session.run db $ do
    segs <- Map.fromListWith (<>) . fmap (\(mid, p, pn) -> ((mid, pn), [p])) <$> Session.statement () ps
    sortOn (maximum . fmap (length . _pu_parts)) .
      groupOn _pu_medium_id .
      map (\p@PartialUpload{..} -> p{_pu_parts=Map.findWithDefault [] (_pu_medium_id, _pu_partnum) segs})
      <$> Session.statement () ups

      where
        ups = Statement "select filename, media_id, upid, did, partnum, chunk_size from uploads order by media_id"
                noParams (rowList partialUploadRow) True
        ps = Statement "select media_id, part, partnum from upload_parts" noParams (rowList pdec) True
        pdec = (,,) <$> (column . nonNullable) Decoders.text
                    <*> (column . nonNullable) int
                    <*> (column . nonNullable) int
        int = fromIntegral <$> Decoders.int8

listQueuedFiles :: MonadIO m => Connection -> m [FilePath]
listQueuedFiles = (fmap . fmap) T.unpack . queryStrings "select filename from uploads"

listToCopyToS3 :: MonadIO m => Connection -> m [MediumID]
listToCopyToS3 = queryStrings sql
  where
    sql = [r|
            select media_id from media
            where media_id not in (select distinct media_id from s3backup)
            order by created_at
            |]

listS3Waiting :: MonadIO m => Connection -> m [String]
listS3Waiting = (fmap . fmap) T.unpack . queryStrings "select filename from s3backup where status is null"

queuedCopyToS3 :: MonadIO m => [(MediumID, String)] -> Connection -> m ()
queuedCopyToS3 ms = mightFail . Session.run (traverse_ (\p -> Session.statement (T.pack <$> p) st) ms)
  where
    st = [resultlessStatement|insert into s3backup (media_id, filename) values ($1::text, $2::text)|]

markS3CopyComplete :: (MonadIO m, ToJSON j) => [(Text, Bool, j)] -> Connection -> m ()
markS3CopyComplete stuffs = mightFail . Session.run (traverse_ (\p -> Session.statement (tr p) st) stuffs)
  where
    tr (fn, ok, res) = (ok, BL.toStrict (J.encode res), fn)
    st = [resultlessStatement|update s3backup set status = $1::bool, response = $2::bytea where filename = $3::text|]

listToCopyLocally :: MonadIO m => Connection -> m [MediumID]
listToCopyLocally = queryStrings "select media_id from media order by created_at"

clearUploads :: MonadIO m => Connection -> m ()
clearUploads = mightFail . Session.run (do
  g "delete from metablob where media_id not in (select media_id from uploads)"
  g "delete from upload_parts"
  g "delete from uploads")
  where
    g q = Session.statement () (Statement q noParams noResult True)

newtype NamedSummary = NamedSummary (MediumID, MDSummary)

namedSummaryRow :: Row NamedSummary
namedSummaryRow = do
  mid <- (column . nonNullable) Decoders.text
  _cameraModel <- (column . nonNullable) (T.unpack <$> Decoders.text)
  _capturedTime <- (column . nullable) Decoders.timestamptz
  _lat <- f
  _lon <- f
  _maxSpeed2d <- f
  _maxSpeed3d <- f
  _maxFaces <- (column . nonNullable) (fromIntegral <$> Decoders.int8)
  loc <- (column . nullable) readr
  prob <- (column . nullable) Decoders.float4
  let _mainScene = liftA2 (,) loc prob
  _maxDistance <- f
  _totDistance <- f
  pure $ NamedSummary (mid, MDSummary{..})

  where
    f = (column . nullable) Decoders.float8

selectMeta :: forall m. MonadIO m => Connection -> m (Map MediumID MDSummary)
selectMeta = fmap (Map.fromList . fmap (\(NamedSummary x) -> x)) . mightFail . Session.run (Session.statement () st)
  where
    st = Statement sql noParams (rowList namedSummaryRow) True
    sql = [r|
           select media_id::text, camera_model, captured_at, lat, lon,
                  max_speed_2d, max_speed_3d,
                  max_faces, main_scene, main_scene_prob,
                  max_distance, total_distance
           from meta
           where camera_model is not null|]

loadMeta :: forall m. MonadIO m => Connection -> MediumID -> m (Maybe MDSummary)
loadMeta db m = mightFail . Session.run (Session.statement m st) $ db
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) (rowMaybe (unName <$> namedSummaryRow)) True
    sql = [r|select media_id, camera_model, captured_at, lat, lon,
                    max_speed_2d, max_speed_3d,
                    max_faces, main_scene, main_scene_prob,
                    max_distance, total_distance
             from meta
             where media_id = $1 |]
    unName (NamedSummary (_,b)) = b

updateAuth :: MonadIO m => Connection -> AuthInfo -> m ()
updateAuth db AuthInfo{..} = mightFail . Session.run (transaction TX.Serializable TX.Write tx) $ db
  where tx = do
          TX.sql "delete from authinfo"
          TX.statement (_resource_owner_id, _access_token, _refresh_token, fromIntegral _expires_in) st
        st = [resultlessStatement|
                                 insert into authinfo (ts, owner_id, access_token, refresh_token, expires_in)
                                 values (current_timestamp, $1::text, $2::text, $3::text, $4::int)|]

loadAuth :: MonadIO m => Connection -> m AuthResult
loadAuth = mightFail . Session.run (Session.statement () st)
  where
    st = Statement "select access_token, expires_in, refresh_token, owner_id, ts - '30 minutes'::interval + ('1 second'::interval * expires_in) < current_timestamp as expired from authinfo" noParams (singleRow dec) True
    dec = AuthResult <$> decai <*> column (nonNullable Decoders.bool)
    decai = AuthInfo <$> column (nonNullable Decoders.text)
                     <*> (fromIntegral <$> column (nonNullable Decoders.int8))
                     <*> column (nonNullable Decoders.text)
                     <*> column (nonNullable Decoders.text)

-- TODO:  This would be nice.
fixupQuery :: MonadIO m => Connection -> Text -> m [[(Text, J.Value)]]
fixupQuery _ _ = liftIO $ fail "fixup query isn't currently supported for postgres"

loadGPSReadings :: MonadIO m => Connection -> MediumID -> Fold GPSReading b -> m b
loadGPSReadings db m (Fold step a ex) =  mightFail . Session.run (ex <$> Session.statement m st) $ db
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) (foldlRows step a dec) True
    sql = [r|select lat, lon, altitude, speed2d, speed3d, timestamp, dop, fix from routes where media_id = $1 :: text order by timestamp|]
    dec = GPSReading <$> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.timestamptz)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable int)
    int = fromIntegral <$> Decoders.int8

storeGPSReadings :: MonadIO m => Connection -> MediumID -> [GPSReading] -> m ()
storeGPSReadings db mid wps = mightFail . Session.run (transaction TX.Serializable TX.Write tx) $ db
  where
    tx = do
      TX.statement mid (Statement "delete from gps_readings where media_id = $1 :: text" (Encoders.param (Encoders.nonNullable Encoders.text)) noResult True)
      traverse_ (\GPSReading{..} -> TX.statement (mid, _gpsr_time, _gpsr_lat, _gpsr_lon, _gpsr_alt, _gpsr_speed2d, _gpsr_speed3d, _gpsr_dop, fromIntegral _gpsr_fix) ist) wps

    ist = [resultlessStatement|insert into gps_readings (media_id, timestamp, lat, lon, altitude, speed2d, speed3d, dop, fix)
           values ($1 :: text, $2 :: timestamptz, $3 :: float8, $4 :: float8, $5 :: float8, $6 :: float8, $7 :: float8, $8 :: float8, $9 :: int4)|]

gpsReadingsTODO :: MonadIO m => Connection -> m [MediumID]
gpsReadingsTODO = queryStrings [r|select m.media_id from meta m
                                  join metablob mb on (m.media_id = mb.media_id)
                                  where lat is not null
                                  and mb.format = 'GPMF'
                                  and not exists (select 1 from gps_readings where media_id = m.media_id)|]
