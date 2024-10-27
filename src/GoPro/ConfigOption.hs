module GoPro.ConfigOption where

import           Data.List       (find)
import           Data.Text       (Text)

data ConfigOption = CfgBucket | CfgCopySQSQueue | CfgCopyFunc
  deriving (Eq, Ord, Show, Bounded, Enum)

optionStr :: ConfigOption -> Text
optionStr CfgBucket       = "bucket"
optionStr CfgCopySQSQueue = "s3copySQSQueue"
optionStr CfgCopyFunc     = "s3copyfunc"

strOption :: Text -> Maybe ConfigOption
strOption s = find ((== s) . optionStr) [minBound..]
