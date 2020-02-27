{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoPro where

import           Control.Lens
import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), Options (..),
                                         Value (..), defaultOptions,
                                         fieldLabelModifier, genericParseJSON,
                                         (.:))
import           Data.Aeson.Lens        (key, _String)
import           Data.Aeson.Types       (typeMismatch)
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (unpack)
import           Data.Time.Clock        (UTCTime)
import qualified Data.Vector            as V
import           Generics.Deriving.Base (Generic)
import           Network.Wreq           (FormParam (..), Options, asJSON,
                                         asValue, defaults, getWith, header,
                                         postWith, responseBody)

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

authenticate :: MonadIO m => String -> String -> m String
authenticate username password = do
  r <- liftIO (asValue =<< postWith defOpts authURL ["grant_type" := ("password" :: String),
                                                     "client_id" := apiClientID,
                                                     "client_secret" := apiClientSecret,
                                                     "scope" := ("root root:channels public me upload media_library_beta live" :: String),
                                                     "username" := username,
                                                     "password" := password])
  pure $ r ^. responseBody . key "access_token" . _String . to unpack

jsonOpts :: Data.Aeson.Options
jsonOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

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
  _captured_at       :: UTCTime,
  _content_title     :: Maybe String,
  _content_type      :: Maybe String,
  _created_at        :: UTCTime,
  _file_size         :: Maybe Int,
  _gopro_user_id     :: String,
  _media_id          :: String,
  _moments_count     :: Int,
  _on_public_profile :: Bool,
  _play_as           :: String,
  _ready_to_edit     :: Bool,
  _ready_to_view     :: String,
  _resolution        :: Maybe String,
  _source_duration   :: Maybe String,
  _media_type        :: String
  } deriving (Generic, Show)

makeLenses ''Media

instance FromJSON Media where
  parseJSON = genericParseJSON jsonOpts{
    fieldLabelModifier = \x -> case x of
                                 "_media_type" -> "type"
                                 "_media_id"   -> "id"
                                 _             -> dropWhile (== '_') x
    }

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
  r <- jget tok ("https://api.gopro.com/media/search?fields=captured_at,content_title,content_type,created_at,gopro_user_id,file_size,id,moments_count,moments_count,on_public_profile,play_as,ready_to_edit,ready_to_view,source_duration,type,resolution&order_by=captured_at&per_page=" <> show psize <> "&page=" <> show page)
  pure $ (r ^.. media . folded,
          r ^. pages)

-- | List all media.  This only includes items that are "ready to view"
listAll :: MonadIO m => String -> m [Media]
listAll tok = do
  let pageSize = 100
  (m1, pg) <- list tok pageSize 0
  ms <- foldM (\o x -> list tok pageSize x >>= \(m,_) -> pure (o <> m)) m1 [1.. (_total_pages pg)]
  pure $ filter ((== "ready") . _ready_to_view) ms

data File = File {
  _camera_position :: String,
  _height          :: Int,
  _width           :: Int,
  _item_number     :: Int,
  _orientation     :: Int,
  _url             :: String
  } deriving (Generic, Show)

instance FromJSON File where
  parseJSON = genericParseJSON jsonOpts

data FileStuff = FileStuff {
  _files      :: [File],
  _variations :: [Value]
  -- TODO: sprites, sidecar_files
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
