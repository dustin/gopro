{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GoPro.Commands.Web where

import           Cleff
import           Cleff.Fail
import           Cleff.Reader
import           Control.Applicative            (asum, (<|>))
import           Control.Concurrent.STM         (atomically, dupTChan, readTChan)
import qualified Control.Foldl                  as Foldl
import           Control.Lens
import           Control.Monad                  (forever)
import           Control.Monad.Reader           (lift)
import qualified Data.Aeson                     as J
import qualified Data.Aeson.KeyMap              as KM
import           Data.Aeson.Lens                (_Object)
import qualified Data.Aeson.Types               as J
import           Data.Cache                     (insert)
import           Data.Foldable                  (fold)
import           Data.List                      (intercalate)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (fromMaybe)
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import qualified Data.Vector                    as V
import           Network.HTTP.Types.Status      (noContent204)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Middleware.Gzip    as GZ
import           Network.Wai.Middleware.Static  (addBase, noDots, staticPolicy, (>->))
import qualified Network.WebSockets             as WS
import           Numeric
import           System.FilePath.Posix          (takeFileName, (</>))
import           Text.XML.Light
import           UnliftIO                       (async)
import           Web.Scotty.Trans               (ActionT, ScottyT, captureParam, file, get, json, middleware, post, raw,
                                                 scottyAppT, setHeader, status, text)

import           GoPro.Commands
import           GoPro.Commands.Sync            (extractFiles, refreshMedia, runFullSync)
import           GoPro.DB
import           GoPro.DEVC                     (GPSReading (..))
import           GoPro.File
import           GoPro.Logging
import           GoPro.Notification
import           GoPro.Plus.Auth
import           GoPro.Plus.Media
import           GoPro.Resolve
import           GoPro.S3


ltshow :: Show a => a -> LT.Text
ltshow = LT.pack . show

namedFiles :: Medium -> Int -> (a -> FileData) -> [a] -> [(String, a)]
namedFiles Medium{..} mx fdf = fmap nameOne
  where
    names = maybe mempty (Map.fromList . zip [1 .. mx] . iterate nextFile) (parseGPFileName =<< _medium_filename)
    nameOne a = (maybe (fromMaybe "" _medium_filename) (takeFileName . _gpFilePath) $ Map.lookup _fd_item_num names, a)
      where
        FileData{..} = fdf a

runServer :: forall es. [Reader Env, LogFX, S3, DatabaseEff, Fail, IOE] :>> es => Eff es ()
runServer = do
  let settings = Warp.setPort 8008 Warp.defaultSettings
  env <- ask
  withRunInIO \runIO -> do
    app <- scottyAppT runIO (application env)
    runIO $ logInfo "Starting web server at http://localhost:8008/"
    liftIO $ Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsapp env) app

  where
    wsapp :: Env -> WS.ServerApp
    wsapp Env{noteChan} pending = do
      ch <- atomically $ dupTChan noteChan
      conn <- WS.acceptRequest pending
      WS.withPingThread conn 30 (pure ()) $
        forever (WS.sendTextData conn . J.encode =<< (atomically . readTChan) ch)

    application :: Env -> ScottyT (Eff es) ()
    application env = do
      let staticPath = optStaticPath . gpOptions $ env
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

      get "/api/files" do
        ms <- lift loadMedia
        fs <- Map.fromListWith (<>) . fmap (\x -> (_fd_medium x, [x])) <$> lift (loadFiles Nothing)
        json $ map (\m@Medium{..} -> fromMaybe (J.toJSON m) $ do
                    basename <- parseGPFileName =<< _medium_filename
                    fd <- sources <$> Map.lookup _medium_id fs
                    let names = Map.fromList $ zip [1 .. length fd] (iterate nextFile basename)
                        numbered n = maybe (fromMaybe "" _medium_filename) (takeFileName . _gpFilePath) $ Map.lookup n names
                        name d@FileData{..} = J.toJSON d & _Object . at "filename" ?~ J.toJSON (numbered _fd_item_num)
                        j = J.toJSON m
                    pure (j & _Object . at "files" ?~ J.listValue name fd)
                   ) ms

      post "/api/sync" do
        _ <- lift . async $ do
          runFullSync
          sendNotification (Notification NotificationReload "" "")
        status noContent204

      post "/api/refresh/:id" do
        imgid <- captureParam "id"
        lift . logInfoL $ ["Refreshing ", imgid]
        lift (refreshMedia (imgid :| []))
        status noContent204

      post "/api/reauth" do
        lift do
          res <- refreshAuth . arInfo =<< loadAuth
          -- Replace the DB value
          updateAuth res
          -- Replace the cache value
          cache <- asks authCache
          liftIO (insert cache () res)
          logInfo "Refreshed auth"
        status noContent204

      get "/thumb/:id" do
        i <- captureParam "id"
        lift (loadThumbnail i) >>= \case
          Nothing ->
            file $ staticPath </> "nothumb.jpg"
          Just b -> do
            setHeader "Content-Type" "image/jpeg"
            setHeader "Cache-Control" "max-age=86400"
            raw b

      get "/api/areas" $ lift selectAreas >>= json

      get "/api/retrieve/:id" do
        imgid <- captureParam "id"
        json @J.Value =<< lift (retrieve imgid)

      get "/api/files/:id" do
        imgid <- captureParam "id"
        Just med <- lift $ loadMedium imgid
        mfiles <- Map.fromList . fmap (\FileData{..} -> ((_fd_section, _fd_label, _fd_item_num), _fd_file_size)) <$> lift (loadFiles (Just imgid))
        let adjustSize f@FileData{..} = f{_fd_file_size=Map.findWithDefault 0 (_fd_section, _fd_label, _fd_item_num) mfiles}
        fs <- extractFiles imgid <$> lift (retrieve imgid)
        let named = namedFiles med (length fs) (\(_,_,nf) -> nf) fs
        json $ fmap (\(fn, (h, u, fd)) -> J.object [
                        ("url", J.toJSON u),
                        ("head", J.toJSON h),
                        ("fileData", J.toJSON (adjustSize fd)),
                        ("filename", J.toJSON fn)]) named

      get "/api/gpslog/:id" do
        mid <- captureParam "id"
        setHeader "Content-Type" "text/csv"
        setHeader "Content-Disposition" ("attachment; filename=\"" <> LT.fromStrict mid <> ".csv\"")
        text =<< lift (foldGPSReadings mid 1000 (Foldl.Fold (\o GPSReading{..} ->
                                                         LT.intercalate "," [
                                                               ltshow _gpsr_time,
                                                               ltshow _gpsr_lat,
                                                               ltshow _gpsr_lon,
                                                               ltshow _gpsr_alt,
                                                               ltshow _gpsr_speed2d,
                                                               ltshow _gpsr_speed3d,
                                                               ltshow _gpsr_dop,
                                                               ltshow _gpsr_fix] : o) ["time,lat,lon,alt,speed2d,speed3d,dop,fix"] (LT.intercalate "\n" . reverse)))

      get "/api/gpspath/:id" do
        gpsExport "application/gpx+xml" "gpx" (\med meta -> Foldl.Fold kmlStep [] (kmlDone med meta)) =<< captureParam "id"

      get "/api/gpxpath/:id" do
        gpsExport "application/gpx+xml" "gpx" (\med _ -> Foldl.Fold gpxStep [] (gpxDone med)) =<< captureParam "id"

      get "/api/retrieve2/:id" do
        imgid <- captureParam "id"
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

    gpsExport :: LT.Text -> LT.Text -> (Medium -> MDSummary -> Foldl.Fold GPSReading LT.Text) -> MediumID -> ActionT (Eff es) ()
    gpsExport mime ext f mid = do
      Just med <- lift $ loadMedium mid
      Just meta <- lift $ loadMeta mid
      setHeader "Content-Type" mime
      setHeader "Content-Disposition" ("attachment; filename=\"" <> LT.fromStrict mid <> "." <> ext <> "\"")
      text =<< lift (foldGPSReadings mid 50 (f med meta))


sources :: [FileData] -> [FileData]
sources fd = asum [filter ((== "source") . _fd_label) fd] -- add some extras here

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
