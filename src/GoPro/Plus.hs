{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoPro.Plus where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), Options (..),
                                         ToJSON (..), Value (..),
                                         defaultOptions, fieldLabelModifier,
                                         genericParseJSON, genericToEncoding,
                                         (.:))
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Map.Strict        as Map
import           Data.Time.Clock        (UTCTime)
import qualified Data.Vector            as V
import           Generics.Deriving.Base (Generic)
import           Network.Wreq           (FormParam (..), Options, asJSON,
                                         defaults, deleteWith, getWith, header,
                                         postWith, putWith, responseBody)
import           System.Random          (getStdRandom, randomR)

import           GoPro.Resolve

userAgent :: BC.ByteString
userAgent = "github.com/dustin/gopro 0.1"

defOpts :: Network.Wreq.Options
defOpts = defaults & header "User-Agent" .~ [userAgent]

apiClientID, apiClientSecret :: String
apiClientID = "71611e67ea968cfacf45e2b6936c81156fcf5dbe553a2bf2d342da1562d05f46"
apiClientSecret = "3863c9b438c07b82f39ab3eeeef9c24fefa50c6856253e3f1d37e0e3b1ead68d"

authURL :: String
authURL = "https://api.gopro.com/v1/oauth2/token"

authOpts :: String -> Network.Wreq.Options
authOpts tok = defOpts & header "Authorization" .~ ["Bearer " <> BC.pack tok]
               & header "Accept" .~ ["application/vnd.gopro.jk.media+json; version=2.0.0"]
               & header "Content-Type" .~ ["application/json"]


jsonOpts :: Data.Aeson.Options
jsonOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

-- | An Authentication response.
data AuthResponse = AuthResponse {
  _access_token    :: String
  , _expires_in    :: Int
  , _refresh_token :: String
  } deriving(Generic, Show)

instance FromJSON AuthResponse where
  parseJSON = genericParseJSON jsonOpts

makeLenses ''AuthResponse

authenticate :: MonadIO m => String -> String -> m AuthResponse
authenticate username password = do
  r <- liftIO (asJSON =<< postWith defOpts authURL ["grant_type" := ("password" :: String),
                                                    "client_id" := apiClientID,
                                                    "client_secret" := apiClientSecret,
                                                    "scope" := ("root root:channels public me upload media_library_beta live" :: String),
                                                    "username" := username,
                                                    "password" := password])
  pure $ r ^. responseBody

-- | Refresh authentication credentials using a refresh token.
refreshAuth :: MonadIO m => AuthResponse -> m AuthResponse
refreshAuth AuthResponse{..} = do
  r <- liftIO ( asJSON =<< postWith defOpts authURL ["grant_type" := ("refresh_token" :: String),
                                                     "client_id" := apiClientID,
                                                     "client_secret" := apiClientSecret,
                                                     "refresh_token" := _refresh_token])
  pure $ r ^. responseBody

data PageInfo = PageInfo {
  _current_page :: Int,
  _per_page     :: Int,
  _total_items  :: Int,
  _total_pages  :: Int
  } deriving (Generic, Show)

makeLenses ''PageInfo

instance FromJSON PageInfo where
  parseJSON = genericParseJSON jsonOpts

{-
failure
transcoding
uploading
-}

data Media = Media {
  _media_id              :: String,
  _media_camera_model    :: Maybe String,
  _media_captured_at     :: UTCTime,
  _media_created_at      :: UTCTime,
  _media_file_size       :: Maybe Int,
  _media_moments_count   :: Int,
  _media_ready_to_view   :: String,
  _media_source_duration :: Maybe String,
  _media_type            :: String,
  _media_token           :: String,
  _media_width           :: Maybe Int,
  _media_height          :: Maybe Int,
  _media_gpmf_data       :: Maybe MDSummary
  } deriving (Generic, Show)

makeLenses ''Media

dropPrefix :: String -> (String -> String)
dropPrefix s = drop (length s)

mediaMod :: String -> String
mediaMod              = dropPrefix "_media_"

instance ToJSON Media where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = mediaMod}

instance FromJSON Media where
  parseJSON = genericParseJSON jsonOpts{ fieldLabelModifier = mediaMod}

-- | Get the thumbnail token for a given media result.
thumbnailURL :: Int -> Media -> String
thumbnailURL n Media{_media_token} = "https://images-0" <> show n <> ".gopro.com/resize/450wwp/" <> _media_token

-- | Proxy a request to GoPro with authentication.
proxy :: MonadIO m => String -> String -> m BL.ByteString
proxy tok u = do
  r <- liftIO $ getWith (authOpts tok) u
  pure $ r ^. responseBody

-- | Fetch thumbnail data for the given media.
fetchThumbnail :: MonadIO m => String -> Media -> m BL.ByteString
fetchThumbnail tok m = do
  n <- liftIO $ getStdRandom (randomR (1,4))
  proxy tok (thumbnailURL n m)

data Listing = Listing {
  _media :: [Media],
  _pages :: PageInfo
  } deriving (Generic, Show)

makeLenses ''Listing

instance FromJSON Listing where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    m <- o .: "media"
    ms <- traverse parseJSON (V.toList m)
    Listing ms <$> v .: "_pages"
  parseJSON invalid    = typeMismatch "Response" invalid


jget :: (MonadIO m, FromJSON a) => String -> String -> m a
jget tok u = view responseBody <$> liftIO (getWith (authOpts tok) u >>= asJSON)

-- | List a page worth of media.
list :: MonadIO m => String -> Int -> Int -> m ([Media], PageInfo)
list tok psize page = do
  r <- jget tok ("https://api.gopro.com/media/search?fields=captured_at,created_at,file_size,id,moments_count,ready_to_view,source_duration,type,token,width,height,camera_model&order_by=created_at&per_page=" <> show psize <> "&page=" <> show page)
  pure $ (r ^.. media . folded,
          r ^. pages)

-- | List all media.
listAll :: MonadIO m => String -> m [Media]
listAll tok = listWhile tok (const True)

-- | List all media while returned batches pass the given predicate.
listWhile :: MonadIO m => String -> ([Media] -> Bool) -> m [Media]
listWhile tok f = do
  Map.elems <$> dig 0 mempty
    where
      dig n m = do
        (ms, _) <- list tok 100 n
        let m' = Map.union m . Map.fromList . map (\md@Media{..} -> (_media_id, md)) $ ms
        if (not . null) ms && f ms
          then dig (n + 1) m'
          else pure m'


data File = File {
  _file_camera_position :: String,
  _file_height          :: Int,
  _file_width           :: Int,
  _file_item_number     :: Int,
  _file_orientation     :: Int,
  _file_url             :: String
  } deriving (Generic, Show)

makeLenses  ''File

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_file_"
    }

data Variation = Variation {
  _var_height  :: Int,
  _var_width   :: Int,
  _var_label   :: String,
  _var_quality :: String,
  _var_type    :: String,
  _var_url     :: String
  } deriving(Generic, Show)

makeLenses ''Variation

instance FromJSON Variation where
  parseJSON = genericParseJSON defaultOptions {
  fieldLabelModifier = dropPrefix "_var_"
  }

data SpriteFrame = SpriteFrame {
  _frame_count  :: Int,
  _frame_height :: Int,
  _frame_width  :: Int
  } deriving(Generic, Show)

makeLenses ''SpriteFrame

instance FromJSON SpriteFrame where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_frame_"
  }

data Sprite = Sprite {
  _sprite_fps    :: Double,
  _sprite_frame  :: SpriteFrame,
  _sprite_height :: Int,
  _sprite_width  :: Int,
  _sprite_type   :: String,
  _sprite_urls   :: [String]
  } deriving (Generic, Show)

instance FromJSON Sprite where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_sprite_"
  }

data FileStuff = FileStuff {
  _files         :: [File],
  _variations    :: [Variation],
  _sprites       :: [Sprite],
  _sidecar_files :: [Value]
  } deriving (Generic, Show)

makeLenses ''FileStuff

instance FromJSON FileStuff where
  parseJSON = genericParseJSON jsonOpts

data FileInfo = FileInfo {
  _fileStuff :: FileStuff,
  _filename  :: String
  } deriving (Generic, Show)

makeLenses ''FileInfo

instance FromJSON FileInfo where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    fs <- parseJSON o
    FileInfo fs <$> v .: "filename"
  parseJSON invalid    = typeMismatch "Response" invalid

dlURL :: String -> String
dlURL k = "https://api.gopro.com/media/" <> k <> "/download"

-- | Retrieve stuff describing a file.
retrieve :: MonadIO m => String -> String -> m FileInfo
retrieve tok k = jget tok (dlURL k)

data Error = Error {
  _error_reason      :: String,
  _error_code        :: Int,
  _error_description :: String,
  _error_id          :: String
  } deriving (Generic, Show)

makeLenses ''Error

instance FromJSON Error where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_error_"
  }

newtype Errors = Errors [Error] deriving (Show)

instance FromJSON Errors where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    e <- o .: "errors"
    Errors <$> parseJSON e
  parseJSON invalid    = typeMismatch "Response" invalid

-- | Delete an item.
delete :: MonadIO m => String -> String -> m [Error]
delete tok k = do
  let u = "https://api.gopro.com/media?ids=" <> k
  Errors r <- view responseBody <$> liftIO (deleteWith (authOpts tok) u >>= asJSON)
  pure r

mediumURL :: String -> String
mediumURL = ("https://api.gopro.com/media/" <>)

rawMedium :: MonadIO m => String -> String -> m Value
rawMedium tok mid = jget tok (mediumURL mid)

putRawMedium :: MonadIO m => String -> String -> Value -> m Value
putRawMedium tok mid v = view responseBody <$> liftIO (putWith (authOpts tok) (mediumURL mid) v >>= asJSON)
