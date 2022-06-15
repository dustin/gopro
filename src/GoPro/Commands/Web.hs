{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GoPro.Commands.Web where

import           Control.Applicative            ((<|>))
import           Control.Concurrent.STM         (atomically, dupTChan, readTChan)
import           Control.Lens
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (ask, asks, lift)
import qualified Data.Aeson                     as J
import qualified Data.Aeson.KeyMap              as KM
import           Data.Aeson.Lens                (_Object)
import           Data.Cache                     (insert)
import           Data.Foldable                  (fold)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map.Strict                as Map
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import qualified Data.Vector                    as V
import           GoPro.AuthDB
import           GoPro.Commands
import           GoPro.Commands.Sync            (refreshMedia, runFullSync)
import           GoPro.DB
import           GoPro.Meta
import           GoPro.Notification
import           GoPro.Plus.Auth
import           GoPro.Plus.Media
import           GoPro.Resolve
import           Network.HTTP.Types.Status      (noContent204)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Middleware.Gzip    as GZ
import           Network.Wai.Middleware.Static  (addBase, noDots, staticPolicy, (>->))
import qualified Network.WebSockets             as WS
import           System.FilePath.Posix          ((</>))
import           UnliftIO                       (async)
import           Web.Scotty.Trans               (ScottyT, file, get, json, middleware, param, post, raw, scottyAppT,
                                                 setHeader, status, text)

ltshow :: Show a => a -> LT.Text
ltshow = LT.pack . show

runServer :: GoPro ()
runServer = do
  env <- ask
  let settings = Warp.setPort 8008 Warp.defaultSettings
  app <- scottyAppT (runIO env) application
  logInfo "Starting web server at http://localhost:8008/"
  liftIO $ Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsapp env) app

  where
    wsapp :: Env -> WS.ServerApp
    wsapp Env{noteChan} pending = do
      ch <- atomically $ dupTChan noteChan
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $
        forever (WS.sendTextData conn . J.encode =<< (atomically . readTChan) ch)

    application :: ScottyT LT.Text GoPro ()
    application = do
      let staticPath = "static"
      middleware $ GZ.gzip GZ.def {GZ.gzipFiles = GZ.GzipCompress}
      middleware $ staticPolicy (noDots >-> addBase staticPath)

      get "/" do
        setHeader "Content-Type" "text/html"
        file $ staticPath </> "index.html"

      get "/api/media" do
        ms <- lift loadMedia
        gs <- lift selectMeta
        json $ map (\m@Medium{..} ->
                      let j = J.toJSON m in
                        case Map.lookup _medium_id gs of
                          Nothing -> j
                          Just g -> let cam = _medium_camera_model <|> Just (_cameraModel g) in
                                      j & _Object . at "camera_model" .~ (J.String .fromString <$> cam)
                                        & _Object . at "meta_data" ?~ J.toJSON g
                   ) ms

      post "/api/sync" do
        _ <- lift . async $ do
          runFullSync
          sendNotification (Notification NotificationReload "" "")
        status noContent204

      post "/api/refresh/:id" do
        imgid <- param "id"
        lift . logInfoL $ ["Refreshing ", imgid]
        lift (refreshMedia (imgid :| []))
        status noContent204

      post "/api/reauth" do
        lift do
          db <- asks dbConn
          res <- refreshAuth =<< loadAuth db
          -- Replace the DB value
          updateAuth db res
          -- Replace the cache value
          cache <- asks authCache
          liftIO (insert cache () res)
          logInfo "Refreshed auth"
        status noContent204

      get "/thumb/:id" $ param "id" >>= lift . loadThumbnail >>= \case
        Nothing ->
          file $ staticPath </> "nothumb.jpg"
        Just b -> do
          setHeader "Content-Type" "image/jpeg"
          setHeader "Cache-Control" "max-age=86400"
          raw b

      get "/api/areas" (lift selectAreas >>= json)

      get "/api/retrieve/:id" do
        imgid <- param "id"
        json @J.Value =<< lift (retrieve imgid)

      get "/api/gpslog/:id" do
        [(GPMF, Just bs)] <- loadMetaBlob =<< param "id"
        readings <- either fail pure $ extractReadings bs
        text $ fold [
          "time,lat,lon,alt,speed2d,speed3d,dilution\n",
          foldMap (\GPSReading{..} ->
                          LT.intercalate "," [
                           ltshow _gps_time,
                           ltshow _gps_lat,
                           ltshow _gps_lon,
                           ltshow _gps_alt,
                           ltshow _gps_speed2d,
                           ltshow _gps_speed3d,
                           ltshow _gps_precision
                           ] <> "\n"
                   ) readings
          ]

      get "/api/retrieve2/:id" do
        imgid <- param "id"
        fi <- _fileStuff <$> lift (retrieve imgid)
        json (encd fi)
          where
            wh w h = T.pack (show w <> "x" <> show h)
            ts = J.String . T.pack
            jn = J.Number . fromIntegral
            encd FileStuff{..} = J.Array . V.fromList . fmap J.Object $ (
              map (\f -> KM.fromList [("url", ts (f ^. file_url)),
                                      ("name", ts "file"),
                                      ("width", jn (f ^. file_width)),
                                      ("height", jn (f ^. file_height)),
                                      ("desc", J.String $ wh (f ^. file_width) (f ^. file_height))]) _files
              <> map (\f -> KM.fromList [("url", ts (f ^. var_url)),
                                         ("name", ts (f ^. var_label)),
                                         ("desc", J.String $ "var " <> wh (f ^. var_width) (f ^. var_height)),
                                         ("width", jn (f ^. var_width)),
                                         ("height", jn (f ^. var_height))]) _variations
              )
