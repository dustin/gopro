{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GoPro.Commands.Web where

import           Control.Applicative            ((<|>))
import           Control.Concurrent.STM         (atomically, dupTChan, readTChan)
import qualified Control.Foldl                  as Foldl
import           Control.Lens
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader, ask, asks, lift)
import qualified Data.Aeson                     as J
import qualified Data.Aeson.KeyMap              as KM
import           Data.Aeson.Lens                (_Object)
import           Data.Cache                     (insert)
import           Data.Foldable                  (fold)
import           Data.List                      (intercalate)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map.Strict                as Map
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import qualified Data.Vector                    as V

import           GoPro.Commands
import           GoPro.Commands.Sync            (refreshMedia, runFullSync)
import           GoPro.DB
import           GoPro.DEVC                     (GPSReading (..))
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
import           Numeric
import           System.FilePath.Posix          ((</>))
import           Text.XML.Light
import           UnliftIO                       (async)
import           Web.Scotty.Trans               (ActionT, ScottyError, ScottyT, file, get, json, middleware, param,
                                                 post, raw, scottyAppT, setHeader, status, text)

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
        Database{..} <- asks database
        ms <- loadMedia
        gs <- selectMeta
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
          Database{..} <- asks database
          res <- refreshAuth . arInfo =<< loadAuth
          -- Replace the DB value
          updateAuth res
          -- Replace the cache value
          cache <- asks authCache
          liftIO (insert cache () res)
          logInfo "Refreshed auth"
        status noContent204

      get "/thumb/:id" do
        i <- param "id"
        db <- asks database
        loadThumbnail db i >>= \case
          Nothing ->
            file $ staticPath </> "nothumb.jpg"
          Just b -> do
            setHeader "Content-Type" "image/jpeg"
            setHeader "Cache-Control" "max-age=86400"
            raw b

      get "/api/areas" $ asks database >>= \Database{..} -> selectAreas >>= json

      get "/api/retrieve/:id" do
        imgid <- param "id"
        json @J.Value =<< lift (retrieve imgid)

      get "/api/gpslog/:id" do
        Database{..} <- asks database
        mid <- param "id"
        setHeader "Content-Type" "text/csv"
        setHeader "Content-Disposition" ("attachment; filename=\"" <> LT.fromStrict mid <> ".csv\"")
        text =<< foldGPSReadings mid 1000 (Foldl.Fold (\o GPSReading{..} ->
                                                         LT.intercalate "," [
                                                               ltshow _gpsr_time,
                                                               ltshow _gpsr_lat,
                                                               ltshow _gpsr_lon,
                                                               ltshow _gpsr_alt,
                                                               ltshow _gpsr_speed2d,
                                                               ltshow _gpsr_speed3d,
                                                               ltshow _gpsr_dop,
                                                               ltshow _gpsr_fix] : o) ["time,lat,lon,alt,speed2d,speed3d,dop,fix"] (LT.intercalate "\n" . reverse))

      get "/api/gpspath/:id" do
        gpsExport "application/gpx+xml" "gpx" (\med meta -> Foldl.Fold kmlStep [] (kmlDone med meta)) =<< param "id"

      get "/api/gpxpath/:id" do
        gpsExport "application/gpx+xml" "gpx" (\med _ -> Foldl.Fold gpxStep [] (gpxDone med)) =<< param "id"

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

gpsExport :: (ScottyError e, MonadReader Env m, MonadIO m) => LT.Text -> LT.Text -> (Medium -> MDSummary -> Foldl.Fold GPSReading LT.Text) -> MediumID -> ActionT e m ()
gpsExport mime ext f mid = do
  Database{..} <- asks database
  Just med <- loadMedium mid
  Just meta <- loadMeta mid
  setHeader "Content-Type" mime
  setHeader "Content-Disposition" ("attachment; filename=\"" <> LT.fromStrict mid <> "." <> ext <> "\"")
  text =<< foldGPSReadings mid 50 (f med meta)

-- XML helpers
elc :: String -> [Attr] -> [Content] -> Element
elc nm atts stuff = Element blank_name{qName= nm} atts stuff Nothing
elr :: String -> [Attr] -> [Element] -> Element
elr nm atts stuff = elc nm atts (Elem <$> stuff)
elt :: String -> String -> Element
elt nm stuff = elc nm [] [xt stuff]
att :: String -> String -> Attr
att k = Attr blank_name{qName=k}
xt :: String -> Content
xt v = Text blank_cdata{cdData=v}

kmlStep :: [String] -> GPSReading -> [String]
kmlStep o GPSReading{..} = intercalate "," [
                           show _gpsr_lon,
                           show _gpsr_lat,
                           show _gpsr_alt] : o

kmlDone :: Medium -> MDSummary -> [String] -> LT.Text
kmlDone Medium{..} MDSummary{..} coords = LT.pack . showTopElement $ kml
  where

    kml = elr "kml" [att "xmlns" "http://www.opengis.net/kml/2.2"] [doc]
    doc = elr "Document" [] [
      elt "name" "GoPro Path",
      elt "description" (fold ["Captured at ", show _medium_captured_at]),
      elr "Style" [ att "id" "yellowLineGreenPoly" ] [
          elr "LineStyle" [] [elt "color" "7f00ffff",
                              elt "width" "4"],
          elr "PolyStyle" [] [elt "color" "7f00ff00"]],
      elr "Placemark" [] [
          elt "name" "Path",
          elt "description" (fold ["Path recorded from the GoPro GPS<br/>",
                                   "Max distance from home: ", maybe "unknown" showf _maxDistance, " m<br/>\n",
                                   "Maximum speed: ", maybe "unknown" (showf . (* 3.6)) _maxSpeed2d, " kph<br/>\n",
                                   "Total distance traveled: ", maybe "unknown" showf _totDistance, " m<br/>\n"
                                  ]),
          elt "styleUrl" "#yellowLineGreenPoly",
          elr "LineString" [] [
              elt "extrude" "1",
              elt "tessellate" "1",
              elt "altitudeMode" "relative",
              elt "coordinates" (intercalate "\n" (reverse coords))
          ]]]

    showf f = showFFloat (Just 2) f ""

gpxStep :: [Element] -> GPSReading -> [Element]
gpxStep acc GPSReading{..} =
            elr "trkpt" [att "lat" (show _gpsr_lat), att "lon" (show _gpsr_lon)] [
              elt "ele" (show _gpsr_alt),
              elt "time" (show _gpsr_time),
              elt "speed" (show _gpsr_speed2d)
              ] : acc

gpxDone :: Medium -> [Element] -> LT.Text
gpxDone Medium{..} els = LT.pack . showTopElement $ gpx
  where
    gpx = elr "gpx" [att "xmlns" "http://www.topografix.com/GPX/1/1"] [doc]
    doc = elr "trk" [] [
      elt "name" ("GoPro Path " <> show _medium_id <> " " <> show _medium_captured_at),
      elr "trkseg" [] (reverse els)
      ]
