{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module GoPro.DB.Sqlite (withSQLite) where

import           Control.Applicative              (liftA2)
import           Control.Foldl                    (Fold (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                       as J
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Coerce                      (coerce)
import           Data.List                        (sortOn)
import           Data.List.Extra                  (groupOn)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (listToMaybe)
import           Data.Scientific                  (fromFloatDigits)
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as TE
import           Data.Typeable                    (Typeable)
import           Database.SQLite.Simple           hiding (bind, close)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.QQ        (sql)
import           Database.SQLite.Simple.ToField


import           Database.SQLite.Simple.FromRow   (fieldWith)
import           GoPro.DB
import           GoPro.DEVC                       (GPSReading (..))
import           GoPro.Plus.Auth                  (AuthInfo (..))
import           GoPro.Plus.Media                 (Medium (..), MediumID, MediumType (..), Moment (..),
                                                   ReadyToViewType (..))
import           GoPro.Plus.Upload                (DerivativeID, Upload (..), UploadPart (..))
import           GoPro.Resolve                    (MDSummary (..))

withSQLite :: String -> (Database -> IO a) -> IO a
withSQLite name a = withConnection name (a . mkDatabase)
  where
    mkDatabase db = Database {
      initTables = GoPro.DB.Sqlite.initTables db,
      loadConfig = GoPro.DB.Sqlite.loadConfig db,
      updateConfig = \m -> GoPro.DB.Sqlite.updateConfig m db,
      updateAuth = GoPro.DB.Sqlite.updateAuth db,
      loadAuth = GoPro.DB.Sqlite.loadAuth db,
      storeMedia = \r -> GoPro.DB.Sqlite.storeMedia r db,
      loadMediaIDs = GoPro.DB.Sqlite.loadMediaIDs db,
      loadMediaRows = GoPro.DB.Sqlite.loadMediaRows db,
      loadMedia = GoPro.DB.Sqlite.loadMedia db,
      loadMedium = GoPro.DB.Sqlite.loadMedium db,
      loadThumbnail = GoPro.DB.Sqlite.loadThumbnail db,
      storeMoments = \mid moms -> GoPro.DB.Sqlite.storeMoments mid moms db,
      loadMoments = GoPro.DB.Sqlite.loadMoments db,
      momentsTODO = GoPro.DB.Sqlite.momentsTODO db,
      metaBlobTODO = GoPro.DB.Sqlite.metaBlobTODO db,
      insertMetaBlob = \m mdt bs -> GoPro.DB.Sqlite.insertMetaBlob m mdt bs db,
      loadMetaBlob = GoPro.DB.Sqlite.loadMetaBlob db,
      selectMetaBlob = GoPro.DB.Sqlite.selectMetaBlob db,
      clearMetaBlob = \ms -> GoPro.DB.Sqlite.clearMetaBlob ms db,
      metaTODO = GoPro.DB.Sqlite.metaTODO db,
      insertMeta = \mid mds -> GoPro.DB.Sqlite.insertMeta mid mds db,
      selectMeta = GoPro.DB.Sqlite.selectMeta db,
      loadMeta = GoPro.DB.Sqlite.loadMeta db,
      storeUpload = \fp mid up did part size -> GoPro.DB.Sqlite.storeUpload fp mid up did part size db,
      completedUploadPart = \mid i p -> GoPro.DB.Sqlite.completedUploadPart mid i p db,
      completedUpload = \mid i -> GoPro.DB.Sqlite.completedUpload mid i db,
      listPartialUploads = GoPro.DB.Sqlite.listPartialUploads db,
      clearUploads = GoPro.DB.Sqlite.clearUploads db,
      listQueuedFiles = GoPro.DB.Sqlite.listQueuedFiles db,
      listToCopyToS3 = GoPro.DB.Sqlite.listToCopyToS3 db,
      queuedCopyToS3 = \stuff -> GoPro.DB.Sqlite.queuedCopyToS3 stuff db,
      markS3CopyComplete = \stuff -> GoPro.DB.Sqlite.markS3CopyComplete stuff db,
      listS3Waiting = GoPro.DB.Sqlite.listS3Waiting db,
      listToCopyLocally = GoPro.DB.Sqlite.listToCopyLocally db,
      selectAreas = GoPro.DB.Sqlite.selectAreas db,
      fixupQuery = GoPro.DB.Sqlite.fixupQuery db,
      foldGPSReadings = GoPro.DB.Sqlite.foldGPSReadings db,
      storeGPSReadings = GoPro.DB.Sqlite.storeGPSReadings db,
      gpsReadingsTODO = GoPro.DB.Sqlite.gpsReadingsTODO db
      }

initQueries :: [(Int, Query)]
initQueries = [
  (1, [sql|create table if not exists media (media_id primary key, camera_model,
                                             captured_at, created_at, file_size,
                                             moments_count, source_duration,
                                             media_type, width, height,
                                             ready_to_view, thumbnail)|]),
  (1, [sql|create table if not exists
           meta (media_id primary key, camera_model, captured_at,
                 lat, lon, max_speed_2d, max_speed_3d, max_faces, main_scene, main_scene_prob)|]),
  (1, "create table if not exists metablob (media_id primary key, meta blob, format text, backedup boolean)"),
  (1, [sql|create table if not exists areas (area_id integer primary key autoincrement,
                                             name text,
                                             lat1 real, lon1 real,
                                             lat2 real, lon2 real)|]),
  (1, "create table if not exists moments (media_id, moment_id integer, timestamp integer)"),
  (1, "create index if not exists moments_by_medium on moments(media_id)"),
  (2, "create table if not exists config (key, value)"),
  (2, "insert into config values ('bucket', '')"),
  (3, "create table if not exists notifications (id integer primary key autoincrement, type text, title text, message text)"),
  (4, "drop table if exists notifications"),
  (5, "alter table media add column variants blob"),
  (6, "create table if not exists uploads (filename, media_id, upid, did, partnum)"),
  (6, "create table if not exists upload_parts (media_id, part)"),
  (7, "alter table upload_parts add column partnum"),
  (8, "create table if not exists s3backup (media_id, filename, status, response)"),
  (9, "insert into config values ('s3copyfunc', 'download-to-s3')"),
  (9, "insert into config values ('s3copySQSQueue', '')"),
  (10, "create unique index if not exists s3backup_by_file on s3backup(filename)"),
  (11, "alter table metablob add column meta_length int"),
  (11, "update metablob set meta_length = length(meta)"),
  (12, "alter table uploads add column chunk_size int"),
  (12, "update uploads set chunk_size = 6291456"),
  (13, "alter table media add column raw_json blob"),
  (14, "alter table media add column filename text"),
  (14, "update media set filename = json_extract(raw_json, \"$.filename\")"),
  (15, "alter table meta add column max_distance real"),
  (15, "alter table meta add column total_distance real"),

  (17, "drop table if exists gps_readings"),

  (18, [sql|create table if not exists gps_readings (
        media_id varchar not null,
        timestamp timestamptz not null,
        lat float8 not null,
        lon float8 not null,
        altitude float8 not null,
        speed2d float not null,
        speed3d float not null,
        dop float not null,
        fix int4 not null)|]),
  (18, "create index gps_readings_by_media_id on gps_readings(media_id)")
  ]

initTables :: MonadIO m => Connection -> m ()
initTables db = liftIO $ do
  [Only uv] <- query_ db "pragma user_version"
  mapM_ (execute_ db) [q | (v,q) <- initQueries, v > uv]
  -- binding doesn't work on this for some reason.  It's safe, at least.
  execute_ db $ "pragma user_version = " <> (fromString . show . maximum . fmap fst $ initQueries)

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
em q rs db = liftIO $ executeMany db q rs

-- execute
ex :: (MonadIO m, ToRow r) => Query -> r -> Connection -> m ()
ex q r db = liftIO $ execute db q r

-- execute_
ex_ :: MonadIO m => Query -> Connection -> m ()
ex_ q db = liftIO $ execute_ db q

jsonToField :: ToJSON a => a -> SQLData
jsonToField v = case toJSON v of
                  J.String x -> SQLText x
                  e          -> error ("wtf is " <> show e)


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
  fromField f = case fieldData f of
                  (SQLText t) -> maybe (returnError ConversionFailed f "invalid value") Ok (strOption t)
                  _           -> returnError ConversionFailed f "invalid type config option"

instance ToField ConfigOption where toField = SQLText . optionStr

instance ToField ReadyToViewType where
  toField = jsonToField

instance ToField MediumType where
  toField = jsonToField

jsonFromField :: (Typeable j, FromJSON j) => String -> (Text -> Text) -> Field -> Ok j
jsonFromField lbl conv f =
  case fieldData f of
    (SQLText t) -> maybee t . J.decode . BL.fromStrict . TE.encodeUtf8 . conv $ t
    _           -> returnError ConversionFailed f ("invalid type for " <> lbl)
  where
    maybee t Nothing  = returnError ConversionFailed f ("invalid value for " <> lbl <> ": " <> show t)
    maybee _ (Just x) = Ok x

quote :: Text -> Text
quote t = "\"" <> t <> "\""

instance FromField ReadyToViewType where
  fromField = jsonFromField "ready to view" quote

instance FromField MediumType where
  fromField = jsonFromField "medium type" quote

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
    toField thumbnail,
    toJSONField vars,
    toJSONField raw
    ]

toJSONField :: ToJSON a => a -> SQLData
toJSONField = toField . TE.decodeUtf8 . BL.toStrict . J.encode

storeMedia :: MonadIO m => [MediaRow] -> Connection -> m ()
storeMedia = em upsertMediaStatement

instance FromRow MediaRow where
  fromRow = MediaRow <$> fromRow <*> field <*> fieldWith (jsonFromField "variants" id) <*> fieldWith (jsonFromField "raw_json" id)

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
  fromField f = case fieldData f of
                  (SQLText "gpmf") -> Ok GPMF
                  (SQLText "exif") -> Ok EXIF
                  (SQLText "")     -> Ok NoMetadata
                  _                -> returnError ConversionFailed f "invalid MetadataType"

instance ToField MetadataType where
  toField GPMF       = SQLText "gpmf"
  toField EXIF       = SQLText "exif"
  toField NoMetadata = SQLText ""

insertMetaBlob :: MonadIO m => MediumID -> MetadataType -> Maybe BS.ByteString -> Connection -> m ()
insertMetaBlob mid fmt blob = ex "insert into metablob (media_id, meta, format, meta_length) values (?, ?, ?, ?)" (
          mid, blob, fmt, maybe 0 BS.length blob)

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
clearMetaBlob = em "update metablob set meta = null, backedup = true where media_id = ?" . fmap Only

insertMeta :: MonadIO m => MediumID -> MDSummary -> Connection -> m ()
insertMeta mid MDSummary{..} = liftIO . up
  where
    q = [sql|
          insert into meta (media_id, camera_model, captured_at, lat, lon,
                            max_speed_2d, max_speed_3d, max_faces,
                            main_scene, main_scene_prob, max_distance, total_distance)
                      values (:mid, :cam, :ts, :lat, :lon,
                              :speed2, :speed3, :maxface,
                              :scene, :scene_prob, :max_distance, :total_distance)
          |]
    up db = executeNamed db q
      [":cam" := _cameraModel,
       ":ts" := _capturedTime,
       ":lat" := _lat,
       ":lon" := _lon,
       ":speed2" := _maxSpeed2d,
       ":speed3" := _maxSpeed3d,
       ":maxface" := _maxFaces,
       ":scene" := (show . fst <$> _mainScene),
       ":scene_prob" := (snd <$> _mainScene),
       ":mid" := mid,
       ":max_distance" := _maxDistance,
       ":total_distance" := _totDistance]

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
clearUploads db = do
  ex_ "delete from metablob where media_id not in (select media_id from media)" db
  ex_ "delete from upload_parts" db
  ex_ "delete from uploads" db

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

loadAuth :: MonadIO m => Connection -> m AuthResult
loadAuth db = liftIO (head <$> query_ db authQuery)

instance FromRow AuthResult where
  fromRow = AuthResult <$> fromRow <*> field

fixupQuery :: MonadIO m => Connection -> Text -> m [[(Text, J.Value)]]
fixupQuery db q = liftIO $ withStatement db (Query q) go
  where
    go st = do
      cnum <- columnCount st
      cols <- traverse (columnName st) [0 .. pred cnum]
      process cols []

        where
          process cols rv = maybe (pure (reverse rv)) (\rs -> process cols (zip cols (resolve <$> rs) : rv)) =<< nextRow st

          resolve (SQLInteger i) = J.Number (fromIntegral i)
          resolve (SQLFloat i)   = J.Number (fromFloatDigits i)
          resolve (SQLText i)    = J.String i
          resolve SQLNull        = J.Null
          resolve (SQLBlob _)    = error "can't do blobs"

instance FromRow GPSReading where
  fromRow = GPSReading <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

foldGPSReadings :: MonadIO m => Connection -> MediumID -> Int -> Fold GPSReading b -> m b
foldGPSReadings db mid maxdop (Fold step a extract) = extract <$> liftIO (fold db q (mid, maxdop) a (\o x -> pure $ step o x))
  where
    q = "select lat, lon, altitude, speed2d, speed3d, timestamp, dop, fix from gps_readings where media_id = ? and dop < ? order by timestamp"

storeGPSReadings :: MonadIO m => Connection -> MediumID -> [GPSReading] -> m ()
storeGPSReadings db mid wps = do
  ex "delete from gps_readings where media_id = ?" (Only mid) db
  let vals = [(mid, _gpsr_time, _gpsr_lat, _gpsr_lon, _gpsr_alt, _gpsr_speed2d, _gpsr_speed3d, _gpsr_dop, _gpsr_fix) | GPSReading{..} <- wps]
  em "insert into gps_readings (media_id, timestamp, lat, lon, altitude, speed2d, speed3d, dop, fix) values (?,?,?,?,?,?,?,?,?)" vals db

gpsReadingsTODO :: MonadIO m => Connection -> m [MediumID]
gpsReadingsTODO = oq_ [sql|select m.media_id from meta m
                                  join metablob mb on (m.media_id = mb.media_id)
                                  where lat is not null
                                  and mb.format = 'gpmf'
                                  and not exists (select 1 from gps_readings where media_id = m.media_id)|]
