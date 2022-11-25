module GoPro.Commands.Fixup where

import           Control.Lens
import           Control.Monad.Fail   (MonadFail (..))
import           Control.Monad.Reader (asks)
import qualified Data.Aeson           as J
import qualified Data.Aeson.Key           as J
import           Data.Aeson.Lens
import           Data.Foldable        (traverse_)
import           Data.List            (partition)
import           Data.Text            (Text)
import           Prelude              hiding (fail)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.Plus.Media

runFixup :: Text -> GoPro ()
runFixup query = asks database >>= \db -> traverse_ store =<< fixupQuery db query
  where
    store stuff = case partition ((== "media_id") . fst) stuff of
                    ([(_, J.String mid)], updates) -> do
                            logInfoL ["Fixing ", tshow mid]
                            updateMedium (\j -> foldr up j updates) mid
                    _ -> fail "no media_id found in result set"

    up :: (Text, J.Value) -> J.Value -> J.Value
    up (k,v) = key (J.fromText k) .~ v
