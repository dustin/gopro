{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoPro where

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
                                         defaults, getWith, header, postWith,
                                         responseBody)
import           System.Random          (getStdRandom, randomR)

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

data Media = Media {
  _media_id          :: String,
  _captured_at       :: UTCTime,
  _content_title     :: Maybe String,
  _content_type      :: Maybe String,
  _created_at        :: UTCTime,
  _file_size         :: Maybe Int,
  _gopro_user_id     :: String,
  _moments_count     :: Int,
  _on_public_profile :: Bool,
  _play_as           :: String,
  _ready_to_edit     :: Bool,
  _ready_to_view     :: String,
  _resolution        :: Maybe String,
  _source_duration   :: Maybe String,
  _media_type        :: String,
  _token             :: String
  } deriving (Generic, Show)

makeLenses ''Media

mediaMod :: String -> String
mediaMod "_media_type" = "type"
mediaMod "_media_id"   = "id"
mediaMod x             = dropWhile (== '_') x

instance ToJSON Media where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = mediaMod}

instance FromJSON Media where
  parseJSON = genericParseJSON jsonOpts{ fieldLabelModifier = mediaMod}

-- | Get the thumbnail token for a given media result.
thumbnailURL :: Int -> Media -> String
thumbnailURL n Media{_token} = "https://images-0" <> show n <> ".gopro.com/resize/450wwp/" <> _token

fetchThumbnail :: MonadIO m => String -> Media -> m BL.ByteString
fetchThumbnail tok m = do
  n <- liftIO $ getStdRandom (randomR (1,4))
  r <- liftIO $ getWith (authOpts tok) (thumbnailURL n m)
  pure $ r ^. responseBody

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
  r <- jget tok ("https://api.gopro.com/media/search?fields=captured_at,content_title,content_type,created_at,gopro_user_id,file_size,id,moments_count,moments_count,on_public_profile,play_as,ready_to_edit,ready_to_view,source_duration,type,resolution,token&order_by=created_at&per_page=" <> show psize <> "&page=" <> show page)
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
    fieldLabelModifier = drop 6
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
  fieldLabelModifier = drop 5
  }

data SpriteFrame = SpriteFrame {
  _frame_count  :: Int,
  _frame_height :: Int,
  _frame_width  :: Int
  } deriving(Generic, Show)

makeLenses ''SpriteFrame

instance FromJSON SpriteFrame where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 7
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
    fieldLabelModifier = drop 8
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

-- | Retrieve stuff describing a file.
retrieve :: MonadIO m => String -> String -> m FileInfo
retrieve tok k = jget tok ("https://api.gopro.com/media/" <> k <> "/download")
