{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB.Sqlite where

import           Control.Applicative              (liftA2)
import           Control.Lens
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (ReaderT (..), MonadReader, ask, asks, runReaderT)
import           Data.Aeson                       (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier,
                                                   genericToEncoding)
import qualified Data.Aeson                       as J
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Coerce                      (coerce)
import           Data.List                        (find, sortOn)
import           Data.List.Extra                  (groupOn)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust, listToMaybe)
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as TE
import           Data.Typeable                    (Typeable)
import           Database.SQLite.Simple           hiding (bind, close)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.QQ        (sql)
import           Control.Monad.Logger                 (LoggingT, MonadLogger)
import           Database.SQLite.Simple.ToField
import           Generics.Deriving.Base           (Generic)
import qualified Control.Monad.Catch                  as E


import           GoPro.Plus.Media                 (FileInfo (..), Medium (..), MediumID, MediumType (..), Moment (..),
                                                   ReadyToViewType (..))
import           GoPro.Plus.Upload                (DerivativeID, Upload (..), UploadID, UploadPart (..))
import           GoPro.Resolve                    (MDSummary (..))
import GoPro.DB

newtype SQLiteEnv = SQLiteEnv { sdb :: Connection }

withSQLiteDB :: Connection -> SQLiteP a -> IO a -- ReaderT SQLiteEnv m a -> m a
withSQLiteDB db a = runReaderT (runSQLiteP a) (SQLiteEnv db)

class Monad m => HasSQLiteDB m where
  goproDB :: m Connection

instance {-# OVERLAPPING #-} Monad m => HasSQLiteDB (ReaderT SQLiteEnv m) where goproDB = asks sdb

newtype SQLiteP a = SqliteP {
  runSQLiteP :: ReaderT SQLiteEnv IO a }
                deriving (Applicative, Functor, Monad, MonadIO, MonadFail,
                          E.MonadThrow, E.MonadCatch, E.MonadMask,
                          MonadReader SQLiteEnv)

instance  Persistence SQLiteP where

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
  (15, "alter table meta add column total_distance real")
  ]

initTables :: Connection -> IO ()
initTables db = do
  [Only uv] <- query_ db "pragma user_version"
  mapM_ (execute_ db) [q | (v,q) <- initQueries, v > uv]
  -- binding doesn't work on this for some reason.  It's safe, at least.
  execute_ db $ "pragma user_version = " <> (fromString . show . maximum . fmap fst $ initQueries)


-- A simple query.
q_ :: (HasSQLiteDB m, MonadIO m, FromRow r) => Query -> m [r]
q_ q = liftIO . flip query_ q =<< goproDB

q' :: (HasSQLiteDB m, MonadIO m, ToRow p, FromRow r) => Query -> p -> m [r]
q' qq p = goproDB >>= \db -> liftIO $ query db qq p

-- A query that returns only a single column.
oq_ :: (HasSQLiteDB m, MonadIO m, FromField r) => Query -> m [r]
oq_ q = unonly <$> q_ q
  where
    unonly :: [Only a] -> [a]
    unonly = coerce

-- execute many
em :: (HasSQLiteDB m, MonadIO m, ToRow r) => Query -> [r] -> m ()
em q rs = goproDB >>= \db -> liftIO $ executeMany db q rs

-- execute
ex :: (HasSQLiteDB m, MonadIO m, ToRow r) => Query -> r -> m ()
ex q r = goproDB >>= \db -> liftIO $ execute db q r

-- execute_
ex_ :: (HasSQLiteDB m, MonadIO m) => Query -> m ()
ex_ q = goproDB >>= \db -> liftIO $ execute_ db q

jsonToField :: ToJSON a => a -> SQLData
jsonToField v = case toJSON v of
                  J.String x -> SQLText x
                  e          -> error ("wtf is " <> show e)


loadConfig :: Connection -> IO (Map ConfigOption Text)
loadConfig db = Map.fromList <$> query_ db "select key, value from config"

updateConfig :: (HasSQLiteDB m, MonadIO m) => Map ConfigOption Text -> m ()
updateConfig cfg = ex_ "delete from config" *> em "insert into config (key, value) values (?,?)" (Map.assocs cfg)

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

jsonFromField :: (Typeable j, FromJSON j) => String -> Field -> Ok j
jsonFromField lbl f =
  case fieldData f of
    (SQLText t) -> Ok . fromJust . J.decode . BL.fromStrict . TE.encodeUtf8 $ ("\"" <> t <> "\"")
    _           -> returnError ConversionFailed f ("invalid type for " <>  lbl)

instance FromField ReadyToViewType where
  fromField = jsonFromField "ready to view"

instance FromField MediumType where
  fromField = jsonFromField "medium type"

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
    toField vars,
    toField raw
    ]


storeMedia :: (HasSQLiteDB m, MonadIO m) => [MediaRow] -> m ()
storeMedia = em upsertMediaStatement

instance FromRow MediaRow where
  fromRow = MediaRow <$> fromRow <*> field <*> field <*> field

loadMediaRows :: (HasSQLiteDB m, MonadIO m) => m [MediaRow]
loadMediaRows = q_ "select media_id, camera_model, captured_at, created_at, file_size, moments_count, ready_to_view, source_duration, media_type, width, height, filename, thumbnail, variants, raw_json from media"

loadMediaIDs :: (HasSQLiteDB m, MonadIO m) => m [MediumID]
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

loadMedia :: (HasSQLiteDB m, MonadIO m) => m [Medium]
loadMedia = q_ selectMediaStatement

loadMedium :: (HasSQLiteDB m, MonadIO m) => MediumID -> m (Maybe Medium)
loadMedium mid = listToMaybe <$> q' selectMediumStatement (Only mid)

loadThumbnail :: (HasSQLiteDB m, MonadIO m) => MediumID -> m (Maybe BL.ByteString)
loadThumbnail imgid = liftIO . sel =<< goproDB
  where
    sel db = do
      [Only t] <- query db "select thumbnail from media where media_id = ?" (Only imgid)
      pure t

metaBlobTODO :: (HasSQLiteDB m, MonadIO m) => m [(MediumID, String)]
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

insertMetaBlob :: (HasSQLiteDB m, MonadIO m) => MediumID -> MetadataType -> Maybe BS.ByteString -> m ()
insertMetaBlob mid fmt blob = ex "insert into metablob (media_id, meta, format, meta_length) values (?, ?, ?, ?)" (
          mid, blob, fmt, maybe 0 BS.length blob)

metaTODO :: (HasSQLiteDB m, MonadIO m) => m [(MediumID, MetadataType, BS.ByteString)]
metaTODO = q_ [sql|
                    select b.media_id, b.format, b.meta
                    from metablob b join media m on (m.media_id = b.media_id)
                    where b.meta is not null
                          and b.media_id not in (select media_id from meta)
                    |]

selectMetaBlob :: (HasSQLiteDB m, MonadIO m) => m [(MediumID, Maybe BS.ByteString)]
selectMetaBlob = q_ "select media_id, meta from metablob where meta is not null"

loadMetaBlob :: (HasSQLiteDB m, MonadIO m) => MediumID -> m (Maybe (MetadataType, Maybe BS.ByteString))
loadMetaBlob mid = listToMaybe <$> q' "select format, meta from metablob where media_id = ?" (Only mid)

clearMetaBlob :: (HasSQLiteDB m, MonadIO m) => [MediumID] -> m ()
clearMetaBlob = em "update metablob set meta = null, backedup = true where media_id = ?" . fmap Only

insertMeta :: (HasSQLiteDB m, MonadIO m) => MediumID -> MDSummary -> m ()
insertMeta mid MDSummary{..} = liftIO . up =<< goproDB
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

selectAreas :: (HasSQLiteDB m, MonadIO m) => m [Area]
selectAreas = q_ "select area_id, name, lat1, lon1, lat2, lon2 from areas"

storeMoments :: (HasSQLiteDB m, MonadIO m) => MediumID -> [Moment] -> m ()
storeMoments mid ms = do
  ex "delete from moments where media_id = ?" (Only mid)
  let vals = [(mid, _moment_id, _moment_time) | Moment{..} <- ms]
  em "insert into moments (media_id, moment_id, timestamp) values (?,?,?)" vals

loadMoments :: (HasSQLiteDB m, MonadIO m) => m (Map MediumID [Moment])
loadMoments = Map.fromListWith (<>) . map (\(a,b,c) -> (a,[Moment b c]))
              <$> q_ "select media_id, moment_id, timestamp from moments"

momentsTODO :: (HasSQLiteDB m, MonadIO m) => m [MediumID]
momentsTODO = oq_ [sql|
                         select m.media_id from media m left outer join
                           (select media_id, count(*) as moco from moments group by media_id) as mo
                           on (m. media_id = mo.media_id)
                          where m.moments_count != ifnull(moco, 0) ;
                         |]

storeUpload :: (HasSQLiteDB m, MonadIO m) => FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> m ()
storeUpload fp mid Upload{..} did partnum chunkSize = do
  ex "insert into uploads (filename, media_id, upid, did, partnum, chunk_size) values (?,?,?,?,?,?)" (
    fp, mid, _uploadID, did, partnum, chunkSize)
  let vals = [(mid, _uploadPart, partnum) | UploadPart{..} <- _uploadParts]
  em "insert into upload_parts (media_id, part, partnum) values (?,?,?)" vals

completedUploadPart :: (HasSQLiteDB m, MonadIO m) => MediumID -> Integer -> Integer -> m ()
completedUploadPart mid i p = ex "delete from upload_parts where media_id = ? and part = ? and partnum = ?" (mid, i, p)

completedUpload :: (HasSQLiteDB m, MonadIO m) => MediumID -> Integer -> m ()
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
listPartialUploads :: (HasSQLiteDB m, MonadIO m) => m [[PartialUpload]]
listPartialUploads = do
    segs <- Map.fromListWith (<>) . fmap (\(mid, p, pn) -> ((mid, pn), [p])) <$>
            q_ "select media_id, part, partnum from upload_parts"
    sortOn (maximum . fmap (length . _pu_parts)) .
      groupOn _pu_medium_id .
      map (\p@PartialUpload{..} -> p{_pu_parts=Map.findWithDefault [] (_pu_medium_id, _pu_partnum) segs})
      <$> q_ "select filename, media_id, upid, did, partnum, chunk_size from uploads order by media_id"

listQueuedFiles :: (HasSQLiteDB m, MonadIO m) => m [FilePath]
listQueuedFiles = oq_ "select filename from uploads"

listToCopyToS3 :: (HasSQLiteDB m, MonadIO m) => m [MediumID]
listToCopyToS3 = oq_ [sql|
                         select media_id from media
                         where media_id not in (select distinct media_id from s3backup)
                         order by created_at
                         |]

listS3Waiting :: (HasSQLiteDB m, MonadIO m) => m [String]
listS3Waiting = oq_ "select filename from s3backup where status is null"

queuedCopyToS3 :: (HasSQLiteDB m, MonadIO m) => [(MediumID, String)] -> m ()
queuedCopyToS3 = em "insert into s3backup (media_id, filename) values (?,?)"

markS3CopyComplete :: (HasSQLiteDB m, MonadIO m, ToJSON j) => [(Text, Bool, j)] -> m ()
markS3CopyComplete stuffs = em "update s3backup set status = ?, response = ? where filename = ?"
                            [(ok, J.encode res, fn) | (fn, ok, res) <- stuffs]

listToCopyLocally :: (HasSQLiteDB m, MonadIO m) => m [MediumID]
listToCopyLocally = oq_ "select media_id from media order by created_at"

clearUploads :: (HasSQLiteDB m, MonadIO m) => m ()
clearUploads = ex_ "delete from upload_parts" *> ex_ "delete from uploads"

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

selectMeta :: forall m. (HasSQLiteDB m, MonadIO m) => m (Map MediumID MDSummary)
selectMeta = Map.fromList . coerce <$> (q_ q :: m [NamedSummary])
  where
    q = [sql|select media_id, camera_model, captured_at, lat, lon,
                    max_speed_2d, max_speed_3d,
                    max_faces, main_scene, main_scene_prob,
                    max_distance, total_distance
             from meta
             where camera_model is not null |]

loadMeta :: forall m. (HasSQLiteDB m, MonadIO m) => MediumID -> m (Maybe MDSummary)
loadMeta m = fmap unName . listToMaybe <$> (q' q (Only m) :: m [NamedSummary])
  where
    q = [sql|select media_id, camera_model, captured_at, lat, lon,
                    max_speed_2d, max_speed_3d,
                    max_faces, main_scene, main_scene_prob,
                    max_distance, total_distance
             from meta
             where media_id = ? |]
    unName (NamedSummary (_,b)) = b
