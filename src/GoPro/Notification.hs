{-# LANGUAGE DeriveGeneric #-}
module GoPro.Notification where

import           Data.Aeson             (FromJSON (..), ToJSON (..),
                                         defaultOptions, fieldLabelModifier,
                                         genericToEncoding)
import qualified Data.Aeson             as J
import           Data.Aeson.Types       (typeMismatch)
import           Data.Char              (toLower, toUpper)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Generics.Deriving.Base (Generic)

data NotificationType = NotificationInfo
    | NotificationError
    | NotificationReload
    deriving (Show, Read)

instance ToJSON NotificationType where
  toJSON = J.String . T.pack . fmap toLower . drop 12 . show

instance FromJSON NotificationType where
  parseJSON (J.String s) = pure . read . trans . T.unpack $ s
    where trans (x:xs) = "Notification" <> (toUpper x : xs)
          trans []     = error "empty notification type"
  parseJSON invalid      = typeMismatch "Response" invalid

data Notification = Notification
    { _note_id      :: Integer
    , _note_type    :: NotificationType
    , _note_title   :: Text
    , _note_message :: Text
    }
    deriving (Show, Generic)

instance ToJSON Notification where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 6}
