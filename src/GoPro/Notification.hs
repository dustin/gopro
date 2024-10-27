{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module GoPro.Notification where

import           Cleff
import           Control.Concurrent.STM (TChan, atomically, dupTChan, writeTChan)
import           Data.Aeson             (FromJSON (..), ToJSON (..), defaultOptions, fieldLabelModifier,
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
    { _note_type    :: NotificationType
    , _note_title   :: Text
    , _note_message :: Text
    }
    deriving (Show, Generic)

instance ToJSON Notification where
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 6}

data NotifyFX :: Effect where
    SendNotification :: Notification -> NotifyFX m ()
    Subscribe :: NotifyFX m (TChan Notification)

makeEffect ''NotifyFX

runNotify :: IOE :> es => TChan Notification -> Eff (NotifyFX : es) a -> Eff es a
runNotify ch = interpret \case
  SendNotification note -> liftIO . atomically . writeTChan ch $ note
  Subscribe -> liftIO . atomically . dupTChan $ ch

-- sendNotification :: [IOE, NotifyFX] :>> es => Notification -> Eff es ()
-- sendNotification note = notify -- asks noteChan >>= \ch -> liftIO . atomically . writeTChan ch $ note

