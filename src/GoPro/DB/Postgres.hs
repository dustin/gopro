{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB.Postgres (runDatabasePostgres, runDatabasePostgresStr) where

import           Cleff

import           Control.Foldl              (Fold (..))
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

import           Hasql.Decoders             (Row, Value, column, foldlRows, foldrRows, noResult, nonNullable, nullable,
                                             rowList, rowMaybe, singleRow)
import qualified Hasql.Decoders             as Decoders
import           Hasql.Encoders             (noParams)
import qualified Hasql.Encoders             as Encoders
import           Hasql.Pool                 (Pool)
import qualified Hasql.Pool                 as Pool
import           Hasql.Session              (Session)
import qualified Hasql.Session              as Session
import           Hasql.Statement            (Statement (..))
import qualified Hasql.Transaction          as TX
import           Hasql.Transaction.Sessions (transaction)
import qualified Hasql.Transaction.Sessions as TX

import           Control.Exception          (throwIO)
import           Control.Monad              ((<=<))
import           Data.Functor.Contravariant ((>$<))
import           GoPro.DB
import           GoPro.DEVC                 (GPSReading (..))
import           GoPro.Plus.Auth            (AuthInfo (..))
import           GoPro.Plus.Media           (Medium (..), MediumID, Moment (..))
import           GoPro.Plus.Upload          (DerivativeID, Upload (..), UploadPart (..))
import           GoPro.Resolve              (MDSummary (..))

runDatabasePostgresStr :: IOE :> es => String -> Eff (DB : es) a -> Eff es a
runDatabasePostgresStr (fromString -> s) f =
  liftIO (Pool.acquire 1 (seconds 1) (seconds 3600) (seconds 900) s) >>= flip runDatabasePostgres f
    where
        seconds = (* 1000000)

-- Effect Interpretation via IO
runDatabasePostgres :: IOE :> es => Pool -> Eff (DB : es) a -> Eff es a
runDatabasePostgres pool = interpretIO \case
  InitTables -> pooling GoPro.DB.Postgres.initTables
  LoadConfig -> pooling GoPro.DB.Postgres.loadConfig
  UpdateConfig config -> pooling (GoPro.DB.Postgres.updateConfig config)

  UpdateAuth authInfo -> pooling (GoPro.DB.Postgres.updateAuth authInfo)
  LoadAuth -> pooling GoPro.DB.Postgres.loadAuth

  StoreMedia mediaRows -> pooling (GoPro.DB.Postgres.storeMedia mediaRows)
  LoadMediaIDs -> pooling GoPro.DB.Postgres.loadMediaIDs
  LoadMediaRows -> pooling GoPro.DB.Postgres.loadMediaRows
  LoadMedia -> pooling GoPro.DB.Postgres.loadMedia
  LoadMedium mediumID -> pooling (GoPro.DB.Postgres.loadMedium mediumID)
  LoadThumbnail mediumID -> pooling (GoPro.DB.Postgres.loadThumbnail mediumID)
  StoreMoments mediumID moments -> pooling (GoPro.DB.Postgres.storeMoments mediumID moments)
  LoadMoments -> pooling GoPro.DB.Postgres.loadMoments
  MomentsTODO -> pooling GoPro.DB.Postgres.momentsTODO
  MetaBlobTODO -> pooling GoPro.DB.Postgres.metaBlobTODO
  InsertMetaBlob mediumID metadataType bs -> pooling (GoPro.DB.Postgres.insertMetaBlob mediumID metadataType bs)
  LoadMetaBlob mediumID -> pooling (GoPro.DB.Postgres.loadMetaBlob mediumID)
  SelectMetaBlob -> pooling GoPro.DB.Postgres.selectMetaBlob
  ClearMetaBlob mediumIDs -> pooling (GoPro.DB.Postgres.clearMetaBlob mediumIDs)
  MetaTODO -> pooling GoPro.DB.Postgres.metaTODO
  InsertMeta mediumID mdSummary -> pooling (GoPro.DB.Postgres.insertMeta mediumID mdSummary)
  SelectMeta -> pooling GoPro.DB.Postgres.selectMeta
  LoadMeta mediumID -> pooling (GoPro.DB.Postgres.loadMeta mediumID)
  StoreUpload filePath mediumID upload derivativeID i1 i2 -> pooling (GoPro.DB.Postgres.storeUpload filePath mediumID upload derivativeID i1 i2)
  CompletedUploadPart mediumID i1 i2 -> pooling (GoPro.DB.Postgres.completedUploadPart mediumID i1 i2)
  CompletedUpload mediumID i -> pooling (GoPro.DB.Postgres.completedUpload mediumID i)
  ListPartialUploads -> pooling GoPro.DB.Postgres.listPartialUploads
  ClearUploads -> pooling GoPro.DB.Postgres.clearUploads
  ListQueuedFiles -> pooling GoPro.DB.Postgres.listQueuedFiles
  ListToCopyToS3 -> pooling GoPro.DB.Postgres.listToCopyToS3
  QueuedCopyToS3 pairs -> pooling (GoPro.DB.Postgres.queuedCopyToS3 pairs)
  MarkS3CopyComplete results -> pooling (GoPro.DB.Postgres.markS3CopyComplete results)
  ListS3Waiting -> pooling GoPro.DB.Postgres.listS3Waiting
  ListToCopyLocally -> pooling GoPro.DB.Postgres.listToCopyLocally
  SelectAreas -> pooling GoPro.DB.Postgres.selectAreas
  DeleteMedia mediaIDs -> pooling (GoPro.DB.Postgres.deleteMedia mediaIDs)

  FoldGPSReadings mediumID maxDop fold -> pooling (GoPro.DB.Postgres.foldGPSReadings mediumID maxDop fold)
  StoreGPSReadings mediumID readings -> pooling (GoPro.DB.Postgres.storeGPSReadings mediumID readings)
  GPSReadingsTODO -> pooling GoPro.DB.Postgres.gpsReadingsTODO

  FileTODO -> pooling GoPro.DB.Postgres.fileTODO
  StoreFiles files -> pooling (GoPro.DB.Postgres.storeFiles files)
  LoadFiles maybeMediumID -> pooling (GoPro.DB.Postgres.loadFiles maybeMediumID)

  FixupQuery query -> pooling (GoPro.DB.Postgres.fixupQuery query)

  where
    -- use :: Pool -> Session a -> IO (Either UsageError a)
    pooling :: Session a -> IO a
    pooling = must <=< liftIO . Pool.use pool
    must = either throwIO pure

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

  (8, "create index gps_readings_by_media_id on gps_readings(media_id)"),

  (9, "alter table media alter column raw_json type jsonb using (encode(raw_json, 'escape'))::jsonb"),
  (9, "alter table media alter column variants type jsonb using (encode(variants, 'escape'))::jsonb"),

  (10, [r|create table if not exists files (
          media_id varchar not null,
          label text not null,
          type text not null,
          item_number int4 not null,
          file_size int8 not null
        )|]),
  (10, "create index files_by_media_id on files(media_id)"),
  (11, "alter table files add column section text not null")
  ]

initTables :: Session ()
initTables = do
      uv <- Session.statement () $ Statement "select coalesce(current_setting('gopro.version', true)::int, 0) as version"
            noParams (Decoders.singleRow (column (Decoders.nonNullable Decoders.int8))) True
      dbname <- Session.statement () $ Statement "select current_database()" noParams (Decoders.singleRow (column (nonNullable Decoders.bytea))) True
      mapM_ Session.sql [q | (v,q) <- initQueries, v > uv]
      Session.sql $ "set gopro.version to " <> (fromString . show . maximum . fmap fst $ initQueries)
      Session.sql $ "alter database " <> dbname <> " set gopro.version from current"

loadConfig :: Session (Map ConfigOption Text)
loadConfig = Session.statement () st
  where
    st :: Statement () (Map ConfigOption Text)
    st = [foldStatement|select key :: text, value :: text from config|] (Fold f mempty conv)

    f :: Map Text Text -> (Text, Text) -> Map Text Text
    f m (k,v) = Map.insert k v m
    conv = Map.mapKeys (fromMaybe (error "invalid option in db") . strOption)

updateConfig :: Map ConfigOption Text -> Session ()
updateConfig cfg = do
  Session.sql "delete from config"
  traverse_ (flip Session.statement ins . f) $ Map.assocs cfg

    where
      ins = [resultlessStatement|insert into config (key, value) values ($1::text, $2::text)|]
      f (k,v) = (optionStr k, v)

upsertMediaS :: Statement (Text, Maybe Text, UTCTime, UTCTime,
                           Maybe Int64, Int64, Maybe DiffTime, Text,
                           Maybe Int64, Maybe Int64, Text, Maybe Text, Maybe ByteString, J.Value, J.Value) ()
upsertMediaS = [resultlessStatement|insert into media (media_id, camera_model, captured_at, created_at,
                                     file_size, moments_count, source_duration, media_type,
                                     width, height, ready_to_view, filename, thumbnail, variants, raw_json)
                                     values($1 :: text, $2 :: text?, $3 :: timestamptz, $4 :: timestamptz,
                                            $5 :: int8?, $6 :: int8, $7 :: interval?, $8 :: text,
                                            $9 :: int8?, $10 :: int8?, $11 :: text, $12 :: text?, $13 :: bytea?, $14 :: jsonb, $15 :: jsonb)
                              on conflict (media_id)
                                 do update
                                   set moments_count = excluded.moments_count,
                                       ready_to_view = excluded.ready_to_view,
                                       variants = excluded.variants,
                                       raw_json = excluded.raw_json
                              |]

storeMedia :: [MediaRow] -> Session ()
storeMedia = traverse_ one
  where
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
      vars,
      raw
      ) upsertMediaS

    parseMS :: String -> Maybe DiffTime
    parseMS s = fromRational . (% 1000) <$> readMaybe s

mediaRow :: Row MediaRow
mediaRow = MediaRow <$> mediumRow
             <*> column (Decoders.nullable (BL.fromStrict <$> Decoders.bytea))
             <*> column (Decoders.nonNullable Decoders.jsonb)
             <*> column (Decoders.nonNullable Decoders.jsonb)

loadMediaRows :: Session [MediaRow]
loadMediaRows = Session.statement () st
  where
    st = Statement [r|select
                         media_id, camera_model, captured_at, created_at, file_size, moments_count, ready_to_view,
                         (extract(epoch from source_duration)*1000)::integer::text, media_type, width, height, filename, thumbnail,
                         variants, raw_json from media|] noParams (rowList mediaRow) True

queryStrings :: ByteString -> Session [Text]
queryStrings q = Session.statement () l
  where
    l = Statement q noParams (rowList ((column . nonNullable) Decoders.text)) True

loadMediaIDs :: Session [MediumID]
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

loadMedia :: Session [Medium]
loadMedia = Session.statement () st
  where
    st = Statement sql noParams (rowList mediumRow) True
    sql = [r|select media_id,
                    camera_model,
                    captured_at,
                    created_at,
                    file_size,
                    moments_count,
                    ready_to_view,
                    (extract(epoch from source_duration)*1000)::integer::text,
                    media_type,
                    width,
                    height,
                    filename
                 from media
                 order by captured_at desc|]

loadMedium :: MediumID -> Session (Maybe Medium)
loadMedium mid = Session.statement mid st
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) (rowMaybe mediumRow) True
    sql = [r|select media_id,
                    camera_model,
                    captured_at,
                    created_at,
                    file_size,
                    moments_count,
                    ready_to_view,
                    (extract(epoch from source_duration)*1000)::integer::text,
                    media_type,
                    width,
                    height,
                    filename
                 from media
                 where media_id = $1|]

loadThumbnail :: MediumID -> Session (Maybe BL.ByteString)
loadThumbnail imgid = fmap BL.fromStrict <$> Session.statement imgid st
  where
    st = [maybeStatement|select thumbnail::bytea from media where media_id = $1::text|]

metaBlobTODO :: Session [(MediumID, String)]
metaBlobTODO = (fmap.fmap) T.unpack . toList <$> Session.statement () st
  where
    st = [vectorStatement|select media_id::text, media_type::text
                                 from media m
                                 where not exists (select 1 from metablob where media_id = m.media_id)
                                 order by created_at desc|]

tshow :: Show a => a -> Text
tshow = T.pack . show

insertMetaBlob :: MediumID -> MetadataType -> Maybe BS.ByteString -> Session ()
insertMetaBlob mid fmt blob = Session.statement (mid, tshow fmt, blob, fromIntegral $ maybe 0 BS.length blob) st
  where
    st = [resultlessStatement|
            insert into metablob (media_id, format, meta, meta_length, backedup) values ($1::text, $2::text, $3::bytea?, $4::int8, false)
         |]

metaTODO :: Session [(MediumID, MetadataType, BS.ByteString)]
metaTODO = Session.statement () st
  where
    st = Statement sql noParams (rowList dec) True
    dec = (,,) <$> column (nonNullable Decoders.text) <*> column (nonNullable readr) <*> column (nonNullable Decoders.bytea)
    sql = [r|select b.media_id, b.format, b.meta
                    from metablob b join media m using (media_id)
                    where b.meta is not null
                          and not exists (select 1 from meta where media_id = b.media_id)
              limit 20
            |]

selectMetaBlob :: Session [(MediumID, Maybe BS.ByteString)]
selectMetaBlob = Session.statement () st
  where
    st = Statement sql noParams (rowList dec) True
    dec = (,) <$> column (nonNullable Decoders.text) <*> column (nullable Decoders.bytea)
    sql = "select media_id, meta from metablob where meta is not null"

loadMetaBlob :: MediumID -> Session (Maybe (MetadataType, Maybe BS.ByteString))
loadMetaBlob mid = resolve <$> Session.statement mid st
  where
    resolve :: Maybe (Text, Maybe ByteString) -> Maybe (MetadataType, Maybe ByteString)
    resolve Nothing       = Nothing
    resolve (Just (t, b)) = maybe Nothing (\x -> Just (x, b)) . readMaybe . T.unpack $ t
    st = [maybeStatement|select format::text, meta::bytea? from metablob where media_id = $1::text|]

clearMetaBlob :: [MediumID] -> Session ()
clearMetaBlob = traverse_ (`Session.statement` st)
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) noResult True
    sql = "update metablob set meta = null, backedup = true where media_id = $1"

insertMeta :: MediumID -> MDSummary -> Session ()
insertMeta mid MDSummary{..} = Session.statement args ins
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

selectAreas :: Session [Area]
selectAreas = Session.statement () st
  where
    st = Statement "select area_id, name, lat1, lon1, lat2, lon2 from areas" noParams (rowList areaRow) True

deleteMedia :: [MediumID] -> Session ()
deleteMedia ms = transaction TX.Serializable TX.Write tx
  where
    tx = traverse_ one ms

    one i = do
      TX.statement i rmmeta
      TX.statement i rmmoments
      TX.statement i rms3
      TX.statement i rmmedia

    rmmeta = [resultlessStatement|delete from meta where media_id = $1::text|]
    rmmoments = [resultlessStatement|delete from moments where media_id = $1::text|]
    rms3 = [resultlessStatement|delete from s3backup where media_id = $1::text|]
    rmmedia = [resultlessStatement|delete from media where media_id = $1::text|]

storeMoments :: MediumID -> [Moment] -> Session ()
storeMoments mid ms = transaction TX.Serializable TX.Write tx
  where
    tx = do
      TX.statement mid del
      traverse_ (`TX.statement` ins) [(mid, _moment_id, fromIntegral <$> _moment_time) | Moment{..} <- ms]

    del = [resultlessStatement|delete from moments where media_id = $1::text|]
    ins = [resultlessStatement|insert into moments (media_id, moment_id, timestamp) values ($1::text,$2::text,$3::int?)|]

loadMoments :: Session (Map MediumID [Moment])
loadMoments = Session.statement () st
  where
    st = [foldStatement|select media_id::text, moment_id::text, timestamp::int? from moments|] (Fold f mempty id)
    f o (m, i, fmap fromIntegral -> t) = Map.alter (\ml -> Just (Moment i t : fromMaybe [] ml)) m o

momentsTODO :: Session [MediumID]
momentsTODO = queryStrings sql
  where
    sql = [r|
            select m.media_id from media m left outer join
                   (select media_id, count(*) as moco from moments group by media_id) as mo
                           on (m.media_id = mo.media_id)
                        where m.moments_count != coalesce(moco, 0)
            |]

storeUpload :: FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> Session ()
storeUpload fp mid Upload{..} did partnum chunkSize = do
  Session.statement (T.pack fp, mid, _uploadID, did, fromIntegral partnum, fromIntegral chunkSize) upSt
  for_ _uploadParts $ \UploadPart{..} -> Session.statement (mid, fromIntegral _uploadPart, fromIntegral partnum) partSt
  where
    upSt = [resultlessStatement|
             insert into uploads (filename, media_id, upid, did, partnum, chunk_size) values ($1::text, $2::text, $3::text, $4::text, $5::int, $6::int)|]
    partSt = [resultlessStatement|insert into upload_parts (media_id, part, partnum) values ($1::text, $2::int, $3::int)|]

completedUploadPart :: MediumID -> Integer -> Integer -> Session ()
completedUploadPart mid i p = Session.statement (mid, fromIntegral i, fromIntegral p) st
  where
    st :: Statement (Text, Int32, Int32) ()
    st = [resultlessStatement|delete from upload_parts where media_id = $1::text and part = $2::int and partnum = $3::int|]

completedUpload :: MediumID -> Integer -> Session ()
completedUpload mid p = Session.statement (mid, fromIntegral p) st
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
listPartialUploads :: Session [[PartialUpload]]
listPartialUploads = do
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

listQueuedFiles :: Session [FilePath]
listQueuedFiles = fmap T.unpack <$> queryStrings "select filename from uploads"

listToCopyToS3 :: Session [MediumID]
listToCopyToS3 = queryStrings sql
  where
    sql = [r|
            select media_id from media m
            where exists (select 1 from s3backup where media_id = m.media_id)
            order by created_at
            |]

listS3Waiting :: Session [String]
listS3Waiting = fmap T.unpack <$> queryStrings "select filename from s3backup where status is null"

queuedCopyToS3 :: [(MediumID, String)] -> Session ()
queuedCopyToS3 = traverse_ (\p -> Session.statement (T.pack <$> p) st)
  where
    st = [resultlessStatement|insert into s3backup (media_id, filename) values ($1::text, $2::text)|]

markS3CopyComplete :: ToJSON j => [(Text, Bool, j)] -> Session ()
markS3CopyComplete = traverse_ (\p -> Session.statement (tr p) st)
  where
    tr (fn, ok, res) = (ok, BL.toStrict (J.encode res), fn)
    st = [resultlessStatement|update s3backup set status = $1::bool, response = $2::bytea where filename = $3::text|]

listToCopyLocally :: Session [MediumID]
listToCopyLocally = queryStrings "select media_id from media order by created_at"

clearUploads :: Session ()
clearUploads = do
  g "delete from metablob where media_id not in (select media_id from media)"
  g "delete from upload_parts"
  g "delete from uploads"
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

selectMeta :: Session (Map MediumID MDSummary)
selectMeta = Session.statement () st
  where
    st = Statement sql noParams (foldrRows (\(NamedSummary (k,v)) -> Map.insert k v) mempty namedSummaryRow) True
    sql = [r|
           select media_id::text, camera_model, captured_at, lat, lon,
                  max_speed_2d, max_speed_3d,
                  max_faces, main_scene, main_scene_prob,
                  max_distance, total_distance
           from meta
           where camera_model is not null|]

loadMeta :: MediumID -> Session (Maybe MDSummary)
loadMeta m = Session.statement m st
  where
    st = Statement sql (Encoders.param (Encoders.nonNullable Encoders.text)) (rowMaybe (unName <$> namedSummaryRow)) True
    sql = [r|select media_id, camera_model, captured_at, lat, lon,
                    max_speed_2d, max_speed_3d,
                    max_faces, main_scene, main_scene_prob,
                    max_distance, total_distance
             from meta
             where media_id = $1 |]
    unName (NamedSummary (_,b)) = b

updateAuth :: AuthInfo -> Session ()
updateAuth AuthInfo{..} = transaction TX.Serializable TX.Write tx
  where tx = do
          TX.sql "delete from authinfo"
          TX.statement (_resource_owner_id, _access_token, _refresh_token, fromIntegral _expires_in) st
        st = [resultlessStatement|
                                 insert into authinfo (ts, owner_id, access_token, refresh_token, expires_in)
                                 values (current_timestamp, $1::text, $2::text, $3::text, $4::int)|]

loadAuth :: Session AuthResult
loadAuth = Session.statement () st
  where
    st = Statement "select access_token, expires_in, refresh_token, owner_id, ts - '30 minutes'::interval + ('1 second'::interval * expires_in) < current_timestamp as expired from authinfo" noParams (singleRow dec) True
    dec = AuthResult <$> decai <*> column (nonNullable Decoders.bool)
    decai = AuthInfo <$> column (nonNullable Decoders.text)
                     <*> (fromIntegral <$> column (nonNullable Decoders.int8))
                     <*> column (nonNullable Decoders.text)
                     <*> column (nonNullable Decoders.text)

-- TODO:  This would be nice.
fixupQuery :: Text -> Session [[(Text, J.Value)]]
fixupQuery _ = liftIO $ fail "fixup query isn't currently supported for postgres"

foldGPSReadings :: MediumID -> Int -> Fold GPSReading b -> Session b
foldGPSReadings m maxdop (Fold step a ex) = ex <$> Session.statement (m, fromIntegral maxdop) st
  where
    st = Statement sql enc (foldlRows step a dec) True
    sql = [r|select lat, lon, altitude, speed2d, speed3d, timestamp, dop, fix from gps_readings where media_id = $1 :: text and dop < $2 :: int4 order by timestamp|]
    dec = GPSReading <$> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable Decoders.timestamptz)
                   <*> column (nonNullable Decoders.float8)
                   <*> column (nonNullable int)
    enc = (fst >$< Encoders.param (Encoders.nonNullable Encoders.text)) <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int4))
    int = fromIntegral <$> Decoders.int8

storeGPSReadings :: MediumID -> [GPSReading] -> Session ()
storeGPSReadings mid wps = transaction TX.Serializable TX.Write tx
  where
    tx = do
      TX.statement mid (Statement "delete from gps_readings where media_id = $1 :: text" (Encoders.param (Encoders.nonNullable Encoders.text)) noResult True)
      traverse_ (\GPSReading{..} -> TX.statement (mid, _gpsr_time, _gpsr_lat, _gpsr_lon, _gpsr_alt, _gpsr_speed2d, _gpsr_speed3d, _gpsr_dop, fromIntegral _gpsr_fix) ist) wps

    ist = [resultlessStatement|insert into gps_readings (media_id, timestamp, lat, lon, altitude, speed2d, speed3d, dop, fix)
           values ($1 :: text, $2 :: timestamptz, $3 :: float8, $4 :: float8, $5 :: float8, $6 :: float8, $7 :: float8, $8 :: float8, $9 :: int4)|]

gpsReadingsTODO :: Session [MediumID]
gpsReadingsTODO = queryStrings [r|select m.media_id from meta m
                                  join metablob mb on (m.media_id = mb.media_id)
                                  where lat is not null
                                  and mb.format = 'GPMF'
                                  and not exists (select 1 from gps_readings where media_id = m.media_id)|]

fileTODO :: Session [MediumID]
fileTODO = queryStrings sql
  where
    sql = [r|
       select media_id::text
       from media m
       where not exists (select 1 from files where media_id = m.media_id)
       order by created_at desc
      |]

storeFiles :: [FileData] -> Session ()
storeFiles wps = transaction TX.Serializable TX.Write tx
  where
    tx = do
      -- TX.statement mid (Statement "delete from files where media_id = $1 :: text" (Encoders.param (Encoders.nonNullable Encoders.text)) noResult True)
      traverse_ (\FileData{..} -> TX.statement (_fd_medium, _fd_section, _fd_label, _fd_type, fromIntegral _fd_item_num, fromIntegral _fd_file_size) ist) wps

    ist = [resultlessStatement|insert into files (media_id, section, label, type, item_number, file_size)
           values ($1 :: text, $2 :: text, $3 :: text, $4 :: text, $5 :: int4, $6 :: int8)|]

loadFiles :: Maybe MediumID -> Session [FileData]
loadFiles = maybe runAll runOne
  where
    runAll = Session.statement () stAll
    stAll = Statement "select media_id, section, label, type, item_number, file_size from files" noParams (rowList dec) True
    runOne mid = Session.statement mid stOne
    stOne = Statement "select media_id, section, label, type, item_number, file_size from files where media_id = $1 :: text" (Encoders.param (Encoders.nonNullable Encoders.text)) (rowList dec) True

    dec = FileData
        <$> column (nonNullable Decoders.text)
        <*> column (nonNullable Decoders.text)
        <*> column (nonNullable Decoders.text)
        <*> column (nonNullable Decoders.text)
        <*> (fromIntegral <$> column (nonNullable Decoders.int4))
        <*> (fromIntegral <$> column (nonNullable Decoders.int8))

