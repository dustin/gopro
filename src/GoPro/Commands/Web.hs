{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module GoPro.Commands.Web where

import           Control.Applicative           ((<|>))
import           Control.Lens
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Logger          (runLoggingT)
import           Control.Monad.Reader          (ask, asks, lift, runReaderT)
import qualified Data.Aeson                    as J
import           Data.Aeson.Lens               (_Object)
import           Data.Cache                    (insert)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as Map
import           Data.String                   (fromString)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Vector                   as V
import           Network.HTTP.Types.Status     (noContent204)
import qualified Network.Wai.Middleware.Gzip   as GZ
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import           System.FilePath.Posix         ((</>))
import           Web.Scotty.Trans              (ScottyT, file, get, json,
                                                middleware, param, post, raw,
                                                scottyT, setHeader, status)

import           GoPro.AuthDB
import           GoPro.Commands
import           GoPro.Commands.Sync           (runFullSync)
import           GoPro.DB
import           GoPro.Plus.Auth
import           GoPro.Plus.Media
import           GoPro.Resolve

runServer :: GoPro ()
runServer = ask >>= \x -> scottyT 8008 (runIO x) application
  where
    application :: ScottyT LT.Text EnvM ()
    application = do
      let staticPath = "static"
      middleware $ GZ.gzip GZ.def {GZ.gzipFiles = GZ.GzipCompress}
      middleware $ staticPolicy (noDots >-> addBase staticPath)

      get "/" $ do
        setHeader "Content-Type" "text/html"
        file $ staticPath </> "index.html"

      get "/api/media" $ do
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

      post "/api/sync" $ do
        notlog <- lift $ notificationLogger "web sync"
        env <- lift ask
        liftIO $ (runLoggingT (runReaderT runFullSync env) notlog)
        lift $ addNotification NotificationReload "" ""
        status noContent204

      post "/api/refresh/:id" $ do
        imgid <- param "id"
        lift . logInfo $ "Refreshing " <> imgid
        m <- lift (medium imgid)
        lift (storeMedia [MediaRow m mempty])
        status noContent204

      post "/api/reauth" $ do
        lift $ do
          db <- asks dbConn
          res <- refreshAuth =<< loadAuth db
          -- Replace the DB value
          updateAuth db res
          -- Replace the cache value
          cache <- asks authCache
          liftIO (insert cache () res)
          logInfo "Refreshed auth"
        status noContent204

      get "/thumb/:id" $ do
        setHeader "Content-Type" "image/jpeg"
        setHeader "Cache-Control" "max-age=86400"
        raw =<< lift . loadThumbnail =<< param "id"

      get "/api/areas" (lift selectAreas >>= json)

      get "/api/retrieve/:id" $ do
        imgid <- param "id"
        json @J.Value =<< lift (retrieve imgid)

      get "/api/notifications" (lift getNotifications >>= json)

      get "/api/retrieve2/:id" $ do
        imgid <- param "id"
        fi <- _fileStuff <$> lift (retrieve imgid)
        json (encd fi)
          where
            wh w h = T.pack (show w <> "x" <> show h)
            ts = J.String . T.pack
            jn = J.Number . fromIntegral
            encd FileStuff{..} = J.Array . V.fromList . fmap J.Object $ (
              map (\f -> HM.fromList [("url", ts (f ^. file_url)),
                                      ("name", ts "file"),
                                      ("width", jn (f ^. file_width)),
                                      ("height", jn (f ^. file_height)),
                                      ("desc", J.String $ wh (f ^. file_width) (f ^. file_height))]) _files
              <> map (\f -> HM.fromList [("url", ts (f ^. var_url)),
                                         ("name", ts (f ^. var_label)),
                                         ("desc", J.String $ "var " <> wh (f ^. var_width) (f ^. var_height)),
                                         ("width", jn (f ^. var_width)),
                                         ("height", jn (f ^. var_height))]) _variations
              )
