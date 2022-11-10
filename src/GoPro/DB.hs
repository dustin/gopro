{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.DB (MediaRow(..), row_fileInfo, row_media, row_thumbnail, row_variants, row_raw_json,
                 Area(..), area_id, area_name, area_nw, area_se,
                 PartialUpload(..),
                 MetadataType(..),
                 ConfigOption(..), strOption, optionStr,
                 Database(..),
                 AuthResult(..)
                 ) where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier,
                                         genericToEncoding)
import qualified Data.Aeson             as J
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.List              (find)
import           Data.Map.Strict        (Map)
import           Data.Text              (Text)
import           Generics.Deriving.Base (Generic)

import           GoPro.Plus.Auth        (AuthInfo (..))
import           GoPro.Plus.Media       (FileInfo (..), Medium (..), MediumID, Moment (..))
import           GoPro.Plus.Upload      (DerivativeID, Upload (..), UploadID)
import           GoPro.Resolve          (MDSummary (..))

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

data MetadataType = GPMF | EXIF | NoMetadata deriving (Show, Read, Enum, Bounded, Eq)

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

data AuthResult = AuthResult {
  arInfo    :: AuthInfo,
  arExpired :: Bool
  }
  deriving Show

data Database = Database {
  initTables          :: forall m. MonadIO m => m (),
  loadConfig          :: forall m. MonadIO m => m (Map ConfigOption Text),
  updateConfig        :: forall m. MonadIO m => Map ConfigOption Text -> m (),

  updateAuth          :: forall m. MonadIO m => AuthInfo -> m (),
  loadAuth            :: forall m. MonadIO m => m AuthResult,

  storeMedia          :: forall m. MonadIO m => [MediaRow] -> m (),
  loadMediaIDs        :: forall m. MonadIO m => m [MediumID],
  loadMediaRows       :: forall m. MonadIO m => m [MediaRow],
  loadMedia           :: forall m. MonadIO m => m [Medium],
  loadMedium          :: forall m. MonadIO m => MediumID -> m (Maybe Medium),
  loadThumbnail       :: forall m. MonadIO m => MediumID -> m (Maybe BL.ByteString),
  storeMoments        :: forall m. MonadIO m => MediumID -> [Moment] -> m (),
  loadMoments         :: forall m. MonadIO m => m (Map MediumID [Moment]),
  momentsTODO         :: forall m. MonadIO m => m [MediumID],
  metaBlobTODO        :: forall m. MonadIO m => m [(MediumID, String)],
  insertMetaBlob      :: forall m. MonadIO m => MediumID -> MetadataType -> Maybe BS.ByteString -> m (),
  loadMetaBlob        :: forall m. MonadIO m => MediumID -> m (Maybe (MetadataType, Maybe BS.ByteString)),
  selectMetaBlob      :: forall m. MonadIO m => m [(MediumID, Maybe BS.ByteString)],
  clearMetaBlob       :: forall m. MonadIO m => [MediumID] -> m (),
  metaTODO            :: forall m. MonadIO m => m [(MediumID, MetadataType, BS.ByteString)],
  insertMeta          :: forall m. MonadIO m => MediumID -> MDSummary -> m (),
  selectMeta          :: forall m. MonadIO m => m (Map MediumID MDSummary),
  loadMeta            :: forall m. MonadIO m => MediumID -> m (Maybe MDSummary),
  storeUpload         :: forall m. MonadIO m => FilePath -> MediumID -> Upload -> DerivativeID -> Integer -> Integer -> m (),
  completedUploadPart :: forall m. MonadIO m => MediumID -> Integer -> Integer -> m (),
  completedUpload     :: forall m. MonadIO m => MediumID -> Integer -> m (),
  listPartialUploads  :: forall m. MonadIO m => m [[PartialUpload]],
  clearUploads        :: forall m. MonadIO m => m (),
  listQueuedFiles     :: forall m. MonadIO m => m [FilePath],
  listToCopyToS3      :: forall m. MonadIO m => m [MediumID],
  queuedCopyToS3      :: forall m. MonadIO m => [(MediumID, String)] -> m (),
  markS3CopyComplete  :: forall m j. (MonadIO m, ToJSON j) => [(Text, Bool, j)] -> m (),
  listS3Waiting       :: forall m. MonadIO m => m [String],
  listToCopyLocally   :: forall m. MonadIO m => m [MediumID],
  selectAreas         :: forall m. MonadIO m => m [Area],

  fixupQuery          :: forall m. MonadIO m => Text -> m [[(Text, J.Value)]]
  }
