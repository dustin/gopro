{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (storeMedia, loadMediaIDs, loadMedia, loadMedium, loadMediaRows, loadThumbnail,
                 MediaRow(..), row_fileInfo, row_media, row_thumbnail, row_variants, row_raw_json,
                 storeMoments, loadMoments, momentsTODO,
                 metaBlobTODO, insertMetaBlob, loadMetaBlob, selectMetaBlob, clearMetaBlob,
                 metaTODO, insertMeta, selectMeta, loadMeta,
                 Area(..), area_id, area_name, area_nw, area_se, selectAreas,
                 storeUpload, completedUploadPart, completedUpload, listPartialUploads, PartialUpload(..),
                 clearUploads,
                 listQueuedFiles,
                 listToCopyToS3, queuedCopyToS3, markS3CopyComplete, listS3Waiting,
                 listToCopyLocally,
                 MetadataType(..),
                 initTables, ConfigOption(..), strOption, optionStr, loadConfig, updateConfig,
                 Persistence(..)
                 ) where

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
import           Database.SQLite.Simple.ToField
import           Generics.Deriving.Base           (Generic)

import           GoPro.Plus.Media                 (FileInfo (..), Medium (..), MediumID, MediumType (..), Moment (..),
                                                   ReadyToViewType (..))
import           GoPro.Plus.Upload                (DerivativeID, Upload (..), UploadID, UploadPart (..))
import           GoPro.Resolve                    (MDSummary (..))
import           GoPro.Plus.Auth                (AuthInfo (..))
import GoPro.AuthDB

data ConfigOption = CfgBucket | CfgCopySQSQueue | CfgCopyFunc
  deriving (Eq, Ord, Show, Bounded, Enum)

optionStr :: ConfigOption -> Text
optionStr CfgBucket       = "bucket"
optionStr CfgCopySQSQueue = "s3copySQSQueue"
optionStr CfgCopyFunc     = "s3copyfunc"

strOption :: Text -> Maybe ConfigOption
strOption s = find ((== s) . optionStr) [minBound..]

instance J.FromJSON MediaRow where
  parseJSON o = do
    m <- J.parseJSON o
    pure $ MediaRow m Nothing "" (J.encode o)

data MediaRow = MediaRow
    { _row_media     :: Medium
    , _row_thumbnail :: Maybe BL.ByteString
    , _row_variants  :: BL.ByteString
    , _row_raw_json  :: BL.ByteString
    } deriving (Show, Eq)
makeLenses ''MediaRow

row_fileInfo :: Lens' MediaRow (Maybe FileInfo)
row_fileInfo = lens (\(MediaRow _ _ v _) -> J.decode v) (\(MediaRow m t _ r) x -> MediaRow m t (J.encode x) r)

data MetadataType = GPMF | EXIF | NoMetadata deriving (Show, Enum, Bounded, Eq)

data Area = Area
    { _area_id   :: Int
    , _area_name :: String
    , _area_nw   :: (Double, Double)
    , _area_se   :: (Double, Double)
    }
    deriving (Generic, Show)

makeLenses ''Area

instance ToJSON Area where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 6}

data PartialUpload = PartialUpload
  { _pu_filename  :: FilePath
  , _pu_medium_id :: MediumID
  , _pu_upid      :: UploadID
  , _pu_did       :: DerivativeID
  , _pu_partnum   :: Integer
  , _pu_chunkSize :: Integer
  , _pu_parts     :: [Integer]
  }

class MonadIO m => Persistence m where
  initTables :: m ()
  loadConfig :: m (Map ConfigOption Text)
  updateConfig :: Map ConfigOption Text -> m ()

  updateAuth :: AuthInfo -> m ()
  loadAuth :: m AuthResult

  storeMedia :: [MediaRow] -> m ()
  loadMediaIDs :: m [MediumID]
  loadMediaRows :: m [MediaRow]
  loadMedia :: m [Medium]
  loadMedium :: MediumID -> m (Maybe Medium)
  loadThumbnail :: MediumID -> m (Maybe BL.ByteString)
  storeMoments :: MediumID -> [Moment] -> m ()
  loadMoments :: m (Map MediumID [Moment])
  momentsTODO :: m [MediumID]
  metaBlobTODO :: m [(MediumID, String)]
  insertMetaBlob :: MediumID -> MetadataType -> Maybe BS.ByteString -> m ()
  loadMetaBlob :: MediumID -> m (Maybe (MetadataType, Maybe BS.ByteString))
  selectMetaBlob :: m [(MediumID, Maybe BS.ByteString)]
  clearMetaBlob :: [MediumID] -> m ()
  metaTODO :: m [(MediumID, MetadataType, BS.ByteString)]
  insertMeta :: MediumID -> MDSummary -> m ()
  selectMeta :: m (Map MediumID MDSummary)
  loadMeta :: MediumID -> m (Maybe MDSummary)
  storeUpload :: FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> m ()
  completedUploadPart :: MediumID -> Integer -> Integer -> m ()
  completedUpload :: MediumID -> Integer -> m ()
  listPartialUploads :: m [[PartialUpload]]
  clearUploads :: m ()
  listQueuedFiles :: m [FilePath]
  listToCopyToS3 :: m [MediumID]
  queuedCopyToS3 :: [(MediumID, String)] -> m ()
  markS3CopyComplete :: ToJSON j => [(Text, Bool, j)] -> m ()
  listS3Waiting :: m [String]
  listToCopyLocally :: m [MediumID]
  selectAreas :: m [Area]
