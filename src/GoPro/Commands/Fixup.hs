module GoPro.Commands.Fixup where

import           Control.Monad          (when)
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ask, asks)
import qualified Data.Aeson             as J
import qualified Data.ByteString.Lazy   as BL
import qualified Data.HashMap.Strict    as HM
import           Data.Scientific        (fromFloatDigits)
import           Data.String            (fromString)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Database.SQLite.Simple (SQLData (..), Statement, columnCount,
                                         columnName, nextRow, withStatement)
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
  logDbg $ "Query: " <> tshow query
  liftIO $ withStatement db query (runIO env . needful)

    where
      needful :: Statement -> EnvM ()
      needful st = do
        cnum <- liftIO $ columnCount st
        cols <- mapM (liftIO . columnName st) [0 .. pred cnum]
        process cols
          where
            process :: [T.Text] -> EnvM ()
            process cols = do
              r <- liftIO (nextRow st :: IO (Maybe [SQLData]))
              logDbg $ tshow r
              maybe (pure ()) (\rs -> store (zip cols rs) >> process cols) r
            store :: [(T.Text, SQLData)] -> EnvM ()
            store stuff = do
              mid <- case lookup "media_id" stuff of
                       (Just (SQLText m)) -> pure m
                       _ -> fail "no media_id found in result set"
              logInfo $ "Fixing " <> tshow mid
              (J.Object rawm) <- medium mid
              let v = foldr up rawm (filter (\(k,_) -> k /= "media_id") stuff)
              logDbg $ TE.decodeUtf8 . BL.toStrict . J.encode $ v
              putMedium mid (J.Object v)
            up (name, SQLInteger i) = HM.insert name (J.Number (fromIntegral i))
            up (name, SQLFloat i)   = HM.insert name  (J.Number (fromFloatDigits i))
            up (name, SQLText i)    = HM.insert name (J.String i)
            up (name, SQLNull)      = HM.insert name J.Null
            up (_,    SQLBlob _)    = error "can't do blobs"
