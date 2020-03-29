{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module GoPro.Commands.Web where

import           Control.Lens
import           Control.Monad.Reader          (asks)
import           Control.Monad.Reader          (ask, lift)
import qualified Data.Aeson                    as J
import           Data.Aeson.Lens               (_Object)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as Map
import           Data.String                   (fromString)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Vector                   as V
import qualified Network.Wai.Middleware.Gzip   as GZ
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import           System.FilePath.Posix         ((</>))
import           Web.Scotty.Trans              (ScottyT, file, get, json,
                                                middleware, param, raw, scottyT,
                                                setHeader)

import           GoPro.Commands
import           GoPro.DB
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
        db <- lift $ asks dbConn
        ms <- loadMedia db
        gs <- selectMeta db
        json $ map (\m@Medium{..} ->
                      let j = J.toJSON m in
                        case Map.lookup _medium_id gs of
                          Nothing -> j
                          Just g -> let cam = maybe (Just $ _cameraModel g) Just _medium_camera_model in
                                      j & _Object . at "camera_model" .~ (J.String .fromString <$> cam)
                                        & _Object . at "meta_data" .~ Just (J.toJSON g)
                   ) ms

      get "/thumb/:id" $ do
        db <- lift $ asks dbConn
        imgid <- param "id"
        setHeader "Content-Type" "image/jpeg"
        setHeader "Cache-Control" "max-age=86400"
        raw =<< loadThumbnail db imgid

      get "/api/areas" ((lift $ asks dbConn) >>= selectAreas >>= json)

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
