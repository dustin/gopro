{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (storeMedia, loadMediaIDs, loadMedia, loadMediaRows, loadThumbnail,
                 MediaRow(..), row_fileInfo, row_media, row_thumbnail, row_variants,
                 storeMoments, loadMoments, momentsTODO,
                 metaBlobTODO, insertMetaBlob, selectMetaBlob, clearMetaBlob,
                 metaTODO, insertMeta, selectMeta,
                 Area(..), area_id, area_name, area_nw, area_se, selectAreas,
                 HasGoProDB(..),
                 storeUpload, completedUploadPart, completedUpload, listPartialUploads, PartialUpload(..),
                 listQueuedFiles,
                 listToCopyToS3, queuedCopyToS3, markS3CopyComplete, listS3Waiting,
                 listToCopyLocally,
                 MetadataType(..),
                 initTables, loadConfig, updateConfig, withDB) where

import           Control.Applicative              (liftA2)
import           Control.Lens
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader             (ReaderT (..), ask, runReaderT)
import           Data.Aeson                       (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier,
                                                   genericToEncoding)
import qualified Data.Aeson                       as J
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Coerce                      (coerce)
import           Data.List                        (sortOn)
import           Data.List.Extra                  (groupOn)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust)
import           Data.String                      (fromString)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as TE
import           Data.Typeable                    (Typeable)
import           Database.SQLite.Simple           hiding (bind, close)
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField
import           Generics.Deriving.Base           (Generic)
import           Text.RawString.QQ                (r)

import           GoPro.Plus.Media                 (FileInfo (..), Medium (..), MediumID, MediumType (..), Moment (..),
                                                   ReadyToViewType (..))
import           GoPro.Plus.Upload                (DerivativeID, Upload (..), UploadID, UploadPart (..))
import           GoPro.Resolve                    (MDSummary (..))

class Monad m => HasGoProDB m where
  goproDB :: m Connection

instance {-# OVERLAPPING #-} Monad m => HasGoProDB (ReaderT Connection m) where goproDB = ask

withDB :: Connection -> ReaderT Connection m a -> m a
withDB = flip runReaderT

initQueries :: [(Int, Query)]
initQueries = [
  (1, [r|create table if not exists media (media_id primary key, camera_model,
                                           captured_at, created_at, file_size,
                                           moments_count, source_duration,
                                           media_type, width, height,
                                           ready_to_view, thumbnail)|]),
  (1, [r|create table if not exists
         meta (media_id primary key, camera_model, captured_at,
               lat, lon, max_speed_2d, max_speed_3d, max_faces, main_scene, main_scene_prob)|]),
  (1, "create table if not exists metablob (media_id primary key, meta blob, format text, backedup boolean)"),
  (1, [r|create table if not exists areas (area_id integer primary key autoincrement,
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
  (10, "create unique index if not exists s3backup_by_file on s3backup(filename)")
  ]

initTables :: Connection -> IO ()
initTables db = do
  [Only uv] <- query_ db "pragma user_version"
  mapM_ (execute_ db) [q | (v,q) <- initQueries, v > uv]
  -- binding doesn't work on this for some reason.  It's safe, at least.
  execute_ db $ "pragma user_version = " <> (fromString . show . maximum . fmap fst $ initQueries)

loadConfig :: Connection -> IO (Map Text Text)
loadConfig db = Map.fromList <$> query_ db "select key, value from config"

updateConfig :: (HasGoProDB m, MonadIO m) => Map Text Text -> m ()
updateConfig cfg = liftIO . up =<< goproDB
  where up db = do
          execute_ db "delete from config"
          executeMany db "insert into config (key, value) values (?,?)" (Map.assocs cfg)

upsertMediaStatement :: Query
upsertMediaStatement = [r|insert into media (media_id, camera_model, captured_at, created_at,
                                             file_size, moments_count, source_duration, media_type,
                                             width, height, ready_to_view, thumbnail, variants)
                                      values(?,?,?,?,?,?,?,?,?,?,?,?,?)
                            on conflict (media_id)
                               do update
                                 set moments_count = excluded.moments_count,
                                     ready_to_view = excluded.ready_to_view,
                                     variants = excluded.variants
                              |]

data MediaRow = MediaRow
    { _row_media     :: Medium
    , _row_thumbnail :: BL.ByteString
    , _row_variants  :: BL.ByteString
    } deriving (Show)
makeLenses ''MediaRow

jsonToField :: ToJSON a => a -> SQLData
jsonToField v = case toJSON v of
                  J.String x -> SQLText x
                  e          -> error ("wtf is " <> show e)

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
  toRow (MediaRow Medium{..} thumbnail vars) = [
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
    toField thumbnail,
    toField vars
    ]

row_fileInfo :: Lens' MediaRow (Maybe FileInfo)
row_fileInfo = lens (\(MediaRow _ _ v) -> J.decode v) (\(MediaRow m t _) x -> MediaRow m t (J.encode x))

storeMedia :: (HasGoProDB m, MonadIO m) => [MediaRow] -> m ()
storeMedia media = liftIO . up =<< goproDB
  where up db = executeMany db upsertMediaStatement media

instance FromRow MediaRow where
  fromRow = MediaRow <$> fromRow <*> field <*> field

loadMediaRows :: (HasGoProDB m, MonadIO m) => m [MediaRow]
loadMediaRows = coerce <$> (liftIO . sel =<< goproDB)
  where
    sel :: Connection -> IO [MediaRow]
    sel db = query_ db "select media_id, camera_model, captured_at, created_at, file_size, moments_count, ready_to_view, source_duration, media_type, width, height, thumbnail, variants from media"

loadMediaIDs :: (HasGoProDB m, MonadIO m) => m [MediumID]
loadMediaIDs = coerce <$> (liftIO . sel =<< goproDB)
  where
    sel :: Connection -> IO [Only MediumID]
    sel db = query_ db "select media_id from media order by captured_at desc"

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

loadMedia :: (HasGoProDB m, MonadIO m) => m [Medium]
loadMedia = goproDB >>= \db -> liftIO $ query_ db selectMediaStatement

loadThumbnail :: (HasGoProDB m, MonadIO m) => MediumID -> m BL.ByteString
loadThumbnail imgid = liftIO . sel =<< goproDB
  where
    sel db = do
      [Only t] <- query db "select thumbnail from media where media_id = ?" (Only imgid)
      pure t

metaBlobTODO :: (HasGoProDB m, MonadIO m) => m [(MediumID, String)]
metaBlobTODO = liftIO . sel =<< goproDB
  where
    sel db = query_ db [r|select media_id, media_type
                         from media
                         where media_id not in (select media_id from metablob)
                         order by created_at desc|]


data MetadataType = GPMF | EXIF | NoMetadata deriving Show

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

insertMetaBlob :: (HasGoProDB m, MonadIO m) => MediumID -> MetadataType -> Maybe BS.ByteString -> m ()
insertMetaBlob mid fmt blob = liftIO . ins =<< goproDB
  where ins db = execute db "insert into metablob (media_id, meta, format) values (?, ?, ?)" (mid, blob, fmt)

metaTODO :: (HasGoProDB m, MonadIO m) => m [(MediumID, MetadataType, BS.ByteString)]
metaTODO = liftIO . sel =<< goproDB
  where
    sel db = query_ db [r|
                         select b.media_id, b.format, b.meta
                         from metablob b join media m on (m.media_id = b.media_id)
                         where b.meta is not null
                               and b.media_id not in (select media_id from meta)
                         |]

selectMetaBlob :: (HasGoProDB m, MonadIO m) => m [(MediumID, Maybe BS.ByteString)]
selectMetaBlob = liftIO . sel =<< goproDB
  where sel db = query_ db "select media_id, meta from metablob"

clearMetaBlob :: (HasGoProDB m, MonadIO m) => [MediumID] -> m ()
clearMetaBlob ms = liftIO . up =<< goproDB
  where up db = executeMany db "update metablob set meta = null, backedup = true where media_id = ?" [Only m | m <- ms]

insertMeta :: (HasGoProDB m, MonadIO m) => MediumID -> MDSummary -> m ()
insertMeta mid MDSummary{..} = liftIO . up =<< goproDB
  where
    q = [r|
          insert into meta (media_id, camera_model, captured_at, lat, lon,
                            max_speed_2d, max_speed_3d, max_faces,
                            main_scene, main_scene_prob)
                      values (:mid, :cam, :ts, :lat, :lon,
                              :speed2, :speed3, :maxface,
                              :scene, :scene_prob)
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
       ":mid" := mid]

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
    pure $ NamedSummary (mid, MDSummary{..})

selectMeta :: (HasGoProDB m, MonadIO m) => m (Map MediumID MDSummary)
selectMeta = goproDB >>= \db ->  Map.fromList . coerce <$> liftIO (sel db)
  where
    sel :: Connection -> IO [NamedSummary]
    sel db = query_ db [r|select media_id, camera_model, captured_at, lat, lon,
                              max_speed_2d, max_speed_3d,
                              max_faces, main_scene, main_scene_prob
                          from meta
                          where camera_model is not null |]

data Area = Area
    { _area_id   :: Int
    , _area_name :: String
    , _area_nw   :: (Double, Double)
    , _area_se   :: (Double, Double)
    }
    deriving (Generic, Show)

makeLenses ''Area

instance FromRow Area where
  fromRow = do
    _area_id <- field
    _area_name <- field
    _area_nw <- liftA2 (,) field field
    _area_se <- liftA2 (,) field field
    pure Area{..}

instance ToJSON Area where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 6}

selectAreas :: (HasGoProDB m, MonadIO m) => m [Area]
selectAreas = liftIO . sel =<< goproDB
  where
    sel db = query_ db "select area_id, name, lat1, lon1, lat2, lon2 from areas"

storeMoments :: (HasGoProDB m, MonadIO m) => MediumID -> [Moment] -> m ()
storeMoments mid ms = liftIO . up =<< goproDB
  where
    up db = do
      execute db "delete from moments where media_id = ?" (Only mid)
      let vals = [(mid, _moment_id, _moment_time) | Moment{..} <- ms]
      executeMany db "insert into moments (media_id, moment_id, timestamp) values (?,?,?)" vals

loadMoments :: (HasGoProDB m, MonadIO m) => m (Map MediumID [Moment])
loadMoments = goproDB >>= \db -> Map.fromListWith (<>) . map (\(a,b,c) -> (a,[Moment b c])) <$> liftIO (sel db)
  where sel db = query_ db "select media_id, moment_id, timestamp from moments"

momentsTODO :: (HasGoProDB m, MonadIO m) => m [MediumID]
momentsTODO = liftIO . coerce . sel =<< goproDB
  where
    sel :: Connection -> IO [Only MediumID]
    sel db = query_ db [r|
                         select m.media_id from media m left outer join
                           (select media_id, count(*) as moco from moments group by media_id) as mo
                           on (m. media_id = mo.media_id)
                          where m.moments_count != ifnull(moco, 0) ;
                         |]

storeUpload :: (HasGoProDB m, MonadIO m) => FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> m ()
storeUpload fp mid Upload{..} did partnum = liftIO . ins =<< goproDB
  where
    ins db = do
      execute db "insert into uploads (filename, media_id, upid, did, partnum) values (?,?,?,?,?)" (
        fp, mid, _uploadID, did, partnum)
      let vals = [(mid, _uploadPart, partnum) | UploadPart{..} <- _uploadParts]
      executeMany db "insert into upload_parts (media_id, part, partnum) values (?,?,?)" vals

completedUploadPart :: (HasGoProDB m, MonadIO m) => MediumID -> Integer -> Integer -> m ()
completedUploadPart mid i p = liftIO . up =<< goproDB
  where up db = execute db "delete from upload_parts where media_id = ? and part = ? and partnum = ?" (mid, i, p)

completedUpload :: (HasGoProDB m, MonadIO m) => MediumID -> Integer -> m ()
completedUpload mid partnum = liftIO . up =<< goproDB
  where up db = execute db "delete from uploads where media_id = ? and partnum = ?" (mid, partnum)

data PartialUpload = PartialUpload
  { _pu_filename  :: FilePath
  , _pu_medium_id :: MediumID
  , _pu_upid      :: UploadID
  , _pu_did       :: DerivativeID
  , _pu_partnum   :: Integer
  , _pu_parts     :: [Integer]
  }

instance FromRow PartialUpload where
  fromRow =
    PartialUpload <$> field -- fileName
    <*> field -- media_id
    <*> field -- upid
    <*> field -- did
    <*> field -- partnum
    <*> pure []

-- Return in order of least work to do.
listPartialUploads :: (HasGoProDB m, MonadIO m) => m [[PartialUpload]]
listPartialUploads = liftIO . sel =<< goproDB
  where
    sel db = do
      segs <- Map.fromListWith (<>) . fmap (\(mid, p, pn) -> ((mid, pn), [p])) <$>
              query_ db "select media_id, part, partnum from upload_parts"
      sortOn (maximum . fmap (length . _pu_parts)) .
        groupOn _pu_medium_id .
        map (\p@PartialUpload{..} -> p{_pu_parts=Map.findWithDefault [] (_pu_medium_id, _pu_partnum) segs})
        <$> query_ db "select filename, media_id, upid, did, partnum from uploads order by media_id"

listQueuedFiles :: (HasGoProDB m, MonadIO m) => m [FilePath]
listQueuedFiles = liftIO . coerce . sel =<< goproDB
  where
    sel :: Connection -> IO [Only FilePath]
    sel db = query_ db "select filename from uploads"

listToCopyToS3 :: (HasGoProDB m, MonadIO m) => m [MediumID]
listToCopyToS3 = coerce <$> (liftIO . sel =<< goproDB)
  where
    sel :: Connection -> IO [Only MediumID]
    sel db = query_ db [r|
                         select media_id from media
                         where media_id not in (select distinct media_id from s3backup)
                         order by created_at
                         |]

listS3Waiting :: (HasGoProDB m, MonadIO m) => m [String]
listS3Waiting = coerce <$> (liftIO . sel =<< goproDB)
  where
    sel :: Connection -> IO [Only String]
    sel db = query_ db "select filename from s3backup where status is null"

queuedCopyToS3 :: (HasGoProDB m, MonadIO m) => [(MediumID, String)] -> m ()
queuedCopyToS3 stuff = liftIO . ins =<< goproDB
  where ins db = executeMany db "insert into s3backup (media_id, filename) values (?,?)" stuff

markS3CopyComplete :: (HasGoProDB m, MonadIO m, ToJSON j) => [(Text, Bool, j)] -> m ()
markS3CopyComplete stuffs = liftIO . up =<< goproDB
  where
    up db = executeMany db "update s3backup set status = ?, response = ? where filename = ?"
            (fmap (\(fn, ok, res) -> (ok, J.encode res, fn)) stuffs)

listToCopyLocally :: (HasGoProDB m, MonadIO m) => m [MediumID]
listToCopyLocally = coerce <$> (liftIO . sel =<< goproDB)
  where
    sel :: Connection -> IO [Only MediumID]
    sel db = query_ db "select media_id from media order by created_at"

