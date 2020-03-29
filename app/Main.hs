{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Lens                  hiding (argument)
import           Control.Monad                 (when)
import           Control.Monad.Catch           (bracket_)
import           Control.Monad.Fail            (MonadFail (..))
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Logger          (LogLevel (..),
                                                MonadLoggerIO (..),
                                                filterLogger, runStderrLoggingT)
import           Control.Monad.Reader          (ReaderT (..), ask, asks, lift,
                                                runReaderT)
import qualified Data.Aeson                    as J
import           Data.Aeson.Lens               (_Object)
import qualified Data.ByteString.Lazy          as BL
import           Data.Cache                    (newCache)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (intercalate)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Scientific               (fromFloatDigits)
import           Data.String                   (fromString)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.Lazy                as LT
import qualified Data.Vector                   as V
import           Database.SQLite.Simple        (SQLData (..), Statement,
                                                columnCount, columnName,
                                                nextRow, withConnection,
                                                withStatement)
import qualified Network.Wai.Middleware.Gzip   as GZ
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import           Options.Applicative           (Parser, argument, auto,
                                                execParser, fullDesc, help,
                                                helper, info, long, metavar,
                                                option, progDesc, short,
                                                showDefault, some, str,
                                                strOption, switch, value,
                                                (<**>))
import           Prelude                       hiding (fail)
import           System.Clock                  (TimeSpec (..))
import           System.FilePath.Posix         ((</>))
import           System.IO                     (hFlush, hGetEcho, hSetEcho,
                                                stdin, stdout)
import           System.Posix.Files            (fileSize, getFileStatus)
import           Web.Scotty.Trans              (ScottyT, file, get, json,
                                                middleware, param, raw, scottyT,
                                                setHeader)

import           GoPro.AuthDB
import           GoPro.Commands
import           GoPro.Commands.Sync
import           GoPro.DB
import           GoPro.Plus.Auth
import           GoPro.Plus.Media
import           GoPro.Plus.Upload
import           GoPro.Resolve                 (MDSummary (..))

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> option auto (short 'u' <> long "upload-concurrency" <> showDefault <> value 3 <> help "Upload concurrency")
  <*> option auto (short 'd' <> long "download-concurrency" <> showDefault <> value 11 <> help "Download concurrency")
  <*> some (argument str (metavar "cmd args..."))

runCleanup :: GoPro ()
runCleanup = mapM_ rm =<< (filter wanted <$> listAll)
    where
      wanted Medium{..} = _medium_ready_to_view `elem` ["uploading", "failure"]
      rm Medium{..} = do
        liftIO . putStrLn $ "Removing " <> T.unpack _medium_id <> " (" <> _medium_ready_to_view <> ")"
        errs <- delete _medium_id
        when (not $ null errs) . liftIO . putStrLn $ " error: " <> show errs

runAuth :: GoPro ()
runAuth = do
  u <- liftIO (prompt "Enter email: " >> getLine)
  p <- getPass
  db <- asks dbConn
  res <- authenticate u p
  updateAuth db res

  where
    prompt x = putStr x >> hFlush stdout
    withEcho echo action = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    getPass = liftIO $ withEcho False getLine

runReauth :: GoPro ()
runReauth = do
  db <- asks dbConn
  res <- refreshAuth =<< loadAuth db
  updateAuth db res

runIO :: Env -> EnvM a -> IO a
runIO e m = runReaderT (runEnvM m) e

runFixup :: GoPro ()
runFixup = do
  db <- asks dbConn
  env <- ask
  args <- asks (optArgv . gpOptions)
  when (length args /= 1) $ fail "I need a query to run"
  let [query] = fromString <$> args
  logDbg $ "Query: " <> tshow query
  liftIO $ withStatement db query (\st -> runIO env (needful st))

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
                       _ -> fail ("no media_id found in result set")
              logInfo $ "Fixing " <> tshow mid
              (J.Object rawm) <- medium mid
              let v = foldr up rawm (filter (\(k,_) -> k /= "media_id") stuff)
              logDbg $ TE.decodeUtf8 . BL.toStrict . J.encode $ v
              putMedium mid (J.Object v)
            up (name, (SQLInteger i)) = HM.insert name (J.Number (fromIntegral i))
            up (name, (SQLFloat i))   = HM.insert name  (J.Number (fromFloatDigits i))
            up (name, (SQLText i))    = HM.insert name (J.String i)
            up (name, SQLNull)        = HM.insert name J.Null
            up (_,    (SQLBlob _))    = error "can't do blobs"

runUploadFiles :: GoPro ()
runUploadFiles = mapM_ upload =<< asks (optArgv . gpOptions)

  where
    upload fp = runUpload [fp] $ do
      setLogAction (logError . T.pack)
      mid <- createMedium
      did <- createSource 1
      fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
      Upload{..} <- createUpload did 1 (fromInteger fsize)
      logInfo $ "Uploading " <> tshow fp <> " as " <> mid <> ": did=" <> did <> ", upid=" <> _uploadID
      c <- asks (optUploadConcurrency . gpOptions)
      _ <- mapConcurrentlyLimited c uc _uploadParts
      completeUpload _uploadID did 1 fsize
      markAvailable did

        where
          uc up@UploadPart{..} = do
            logDbg . T.pack $ "Uploading part " <> show _uploadPart <> " of " <> fp
            uploadChunk fp up

runUploadMultipart :: GoPro ()
runUploadMultipart = do
  (typ:fps) <- asks (optArgv . gpOptions)
  runUpload fps $ do
    setMediumType (T.pack typ)
    setLogAction (logError . T.pack)
    mid <- createMedium

    did <- createSource (length fps)
    c <- asks (optUploadConcurrency . gpOptions)
    _ <- mapConcurrentlyLimited c (\(fp,n) -> do
              fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
              Upload{..} <- createUpload did n (fromInteger fsize)
              logInfo $ mconcat ["Uploading ", tshow fp, " as ", mid, " part ", tshow n,
                                 ": did=", did, ", upid=", _uploadID]
              _ <- mapConcurrentlyLimited 2 (uc fp) _uploadParts
              completeUpload _uploadID did n fsize
          ) $ zip fps [1..]
    markAvailable did

      where
        uc fp up@UploadPart{..} = do
          logDbg . T.pack $ "Uploading part " <> show _uploadPart <> " of " <> fp
          uploadChunk fp up

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

run :: String -> GoPro ()
run c = fromMaybe (liftIO unknown) $ lookup c cmds
  where
    cmds = [("auth", runAuth),
            ("reauth", runReauth),
            ("sync", runFetch Incremental >> runGetMeta >> runGrokTel),
            ("fetch", runFetch Incremental),
            ("upload", runUploadFiles),
            ("uploadmulti", runUploadMultipart),
            ("fetchall", runFetch Full),
            ("cleanup", runCleanup),
            ("fixup", runFixup),
            ("serve", runServer),
            ("getmeta", runGetMeta),
            ("groktel", runGrokTel)]
    unknown = do
      putStrLn $ "Unknown command: " <> c
      putStrLn "Try one of these:"
      putStrLn $ "    " <> intercalate "\n    " (map fst cmds)

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  withConnection optDBPath (runConn o)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")

    runConn o@Options{..} db = do
      initTables db
      cache <- newCache (Just (TimeSpec 60 0))

      runStderrLoggingT . logfilt $ do
        l <- askLoggerIO
        let o' = o{optArgv = tail optArgv}
        runReaderT (run (head optArgv)) (Env o' db cache l)

        where
          logfilt = filterLogger (\_ -> flip (if optVerbose then (>=) else (>)) LevelDebug)
