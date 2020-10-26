module GoPro.Commands.Fixup where

import           Control.Lens
import           Control.Monad          (when)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ask, asks)
import qualified Data.Aeson             as J
import           Data.Aeson.Lens
import           Data.Scientific        (fromFloatDigits)
import           Data.String            (fromString)
import qualified Data.Text              as T
import           Database.SQLite.Simple (SQLData (..), Statement, columnCount, columnName, nextRow, withStatement)
import           Prelude                hiding (fail)

import           GoPro.Commands
import           GoPro.Plus.Media

runFixup :: GoPro ()
runFixup = do
  db <- asks dbConn
  env <- ask
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "I need a query to run"
  let [query] = fromString <$> args
  logDbgL ["Query: ", tshow query]
  liftIO $ withStatement db query (runIO env . needful)

    where
      needful :: Statement -> GoPro ()
      needful st = do
        cnum <- liftIO $ columnCount st
        cols <- traverse (liftIO . columnName st) [0 .. pred cnum]
        process cols
          where
            process :: [T.Text] -> GoPro ()
            process cols = do
              r <- liftIO (nextRow st :: IO (Maybe [SQLData]))
              logDbg $ tshow r
              maybe (pure ()) (\rs -> store (zip cols rs) >> process cols) r
            store :: [(T.Text, SQLData)] -> GoPro ()
            store stuff = do
              mid <- case lookup "media_id" stuff of
                       (Just (SQLText m)) -> pure m
                       _                  -> fail "no media_id found in result set"
              logInfoL ["Fixing ", tshow mid]
              updateMedium (\j -> foldr up j (filter (\(k,_) -> k /= "media_id") stuff)) mid
            up :: (T.Text, SQLData) -> J.Value -> J.Value
            up (name, SQLInteger i) = _Object . at name ?~ J.Number (fromIntegral i)
            up (name, SQLFloat i)   = _Object . at name ?~ J.Number (fromFloatDigits i)
            up (name, SQLText i)    = _Object . at name ?~ J.String i
            up (name, SQLNull)      = _Object . at name ?~ J.Null
            up (_,    SQLBlob _)    = error "can't do blobs"
