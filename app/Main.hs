{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Main where

import           Conduit
import           Control.Applicative           (Alternative (..), (<|>))
import           Control.Concurrent.Async      (mapConcurrently)
import           Control.Concurrent.QSem       (newQSem, signalQSem, waitQSem)
import           Control.Exception             (bracket_)
import           Control.Lens                  hiding (argument)
import           Control.Monad                 (MonadPlus (..), mzero, when)
import           Control.Monad.Catch           (SomeException (..), catch)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Logger          (Loc (..), LogLevel (..),
                                                LogSource, LogStr, LoggingT,
                                                MonadLogger (..),
                                                MonadLoggerIO (..),
                                                ToLogStr (..), filterLogger,
                                                logDebugN, logErrorN, logInfoN,
                                                monadLoggerLog,
                                                runStderrLoggingT)
import           Control.Monad.Reader          (MonadReader, ReaderT (..), ask,
                                                asks, lift, runReaderT)
import qualified Data.Aeson                    as J
import qualified Data.ByteString               as BS
import           Data.Foldable                 (asum)
import qualified Data.HashMap.Strict           as HM
import           Data.List.Extra               (chunksOf)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (isJust)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Vector                   as V
import           Database.SQLite.Simple        (Connection, withConnection)
import           Network.HTTP.Simple           (getResponseBody, httpSource,
                                                parseRequest)
import qualified Network.Wai.Middleware.Gzip   as GZ
import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import           Options.Applicative           (Parser, argument, execParser,
                                                fullDesc, help, helper, info,
                                                long, metavar, progDesc, short,
                                                showDefault, some, str,
                                                strOption, switch, value,
                                                (<**>))
import           System.Directory              (createDirectoryIfMissing,
                                                doesFileExist, removeFile)
import           System.FilePath.Posix         ((</>))
import           System.IO                     (hFlush, hGetEcho, hSetEcho,
                                                stdin, stdout)
import           Web.Scotty.Trans              (ScottyT, file, get, json,
                                                middleware, param, raw, scottyT,
                                                setHeader)

import           FFMPeg
import           GoPro.AuthDB
import           GoPro.DB
import           GoPro.Plus
import           GoPro.Resolve

data Options = Options {
  optDBPath     :: String,
  optStaticPath :: FilePath,
  optVerbose    :: Bool,
  optArgv       :: [String]
  }

data Env = Env {
  gpOptions :: Options,
  dbConn    :: Connection,
  envLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

newtype EnvM a = EnvM
  { runEnvM :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env)

instance MonadLogger EnvM where
  monadLoggerLog loc src lvl msg = do
    l <- asks envLogger
    liftIO $ l loc src lvl (toLogStr msg)

type GoPro = ReaderT Env (LoggingT IO)

instance MonadPlus (LoggingT IO) where
  mzero = lift mzero

instance Alternative (LoggingT IO) where
  empty = lift empty
  a <|> b = a `catch` \(SomeException _) -> b

options :: Parser Options
options = Options
  <$> strOption (long "dbpath" <> showDefault <> value "gopro.db" <> help "db path")
  <*> strOption (long "static" <> showDefault <> value "static" <> help "static asset path")
  <*> switch (short 'v' <> long "verbose" <> help "enable debug logging")
  <*> some (argument str (metavar "cmd args..."))

logError :: MonadLogger m => T.Text -> m ()
logError = logErrorN

logInfo :: MonadLogger m => T.Text -> m ()
logInfo = logInfoN

logDbg :: MonadLogger m => T.Text -> m ()
logDbg = logDebugN

tshow :: Show a => a -> T.Text
tshow = T.pack . show

mapConcurrentlyLimited :: (Traversable f, Foldable f) => Int -> (a -> IO b) -> f a -> IO (f b)
mapConcurrentlyLimited n f l = newQSem n >>= \q -> mapConcurrently (b q) l
  where b q x = bracket_ (waitQSem q) (signalQSem q) (f x)

data SyncType = Full | Incremental

runSync :: SyncType -> GoPro ()
runSync stype = do
  tok <- getToken
  db <- asks dbConn
  seen <- Set.fromList <$> loadMediaIDs db
  ms <- todo tok seen
  logInfo $ tshow (length ms) <> " new items"
  when (not . null $ ms) $ logDbg $ "new items: " <> tshow (ms ^.. folded . media_id)
  mapM_ (storeSome tok db) $ chunksOf 100 ms

    where resolve tok m = MediaRow m <$> fetchThumbnail tok m
          todo tok seen = filter (\m@Media{..} -> notSeen m && wanted m)
                          <$> listWhile tok (listPred stype)
            where
              notSeen = (`Set.notMember` seen) . _media_id
              listPred Incremental = all notSeen
              listPred Full        = const True
              wanted Media{..} = isJust _media_file_size
                                 && _media_ready_to_view `elem` ["transcoding", "ready"]
          storeSome tok db l = do
            logInfo $ "Storing batch of " <> tshow (length l)
            storeMedia db =<< fetch tok l
          fetch tok = liftIO . mapConcurrentlyLimited 11 (resolve tok)

runGrokTel :: GoPro ()
runGrokTel = do
  db <- asks dbConn
  mapM_ (ud db) =<< gpmfTODO db
    where
      ud db (mid, bs) = do
        logInfo $ "Updating " <> tshow mid
        case summarize <$> parseDEVC bs of
          Left x -> logError $ "Error parsing stuff for " <> tshow mid <> " show " <> tshow x
          Right x -> updateGPMF db mid x

runGetGPMF :: GoPro ()
runGetGPMF = do
  db <- asks dbConn
  needs <- selectGPMFCandidates db
  logInfo $ "Fetching " <> tshow (length needs)
  logDbg $ tshow needs
  mapM_ (process db) needs
    where
      process :: Connection -> String -> GoPro ()
      process db mid = do
        tok <- getToken
        fi <- retrieve tok mid
        let fv :: String -> FilePath -> GoPro (Maybe BS.ByteString)
            fv s p = Just <$> fetchVariant fi mid s p
            fn v = ".cache" </> mid <> "-" <> v <> ".mp4"
        ms <- asum [
          fv "mp4_low" (fn "low"),
          fv "high_res_proxy_mp4" (fn "high"),
          fv "source" (fn "src"),
          pure Nothing]
        case ms of
          Nothing -> insertGPMF db mid Nothing
          Just s -> do
            logInfo $ "GPMD stream for " <> tshow mid <> " is " <> tshow (BS.length s) <> " bytes"
            insertGPMF db mid (Just s)
            -- Clean up in the success case.
            mapM_ (\f -> asum [liftIO (removeFile f), pure ()]) $ map fn ["low", "high", "src"]

      fetchVariant :: FileInfo -> String -> String -> FilePath -> GoPro BS.ByteString
      fetchVariant fi mid var fn = do
        let mu = fi ^? fileStuff . variations . folded . filtered (has (var_label . only var)) . var_url
        case mu of
          Nothing -> empty
          Just u  -> extractGPMD mid =<< dlIf mid var u fn

      dlIf :: String -> String -> String -> FilePath -> GoPro FilePath
      dlIf mid var u dest = (liftIO $ doesFileExist dest) >>=
        \case
          True -> pure dest
          _ -> download mid var u dest

      download :: String -> String -> String -> FilePath -> GoPro FilePath
      download mid var u dest = do
        liftIO $ createDirectoryIfMissing True ".cache"
        logInfo $ "Fetching " <> tshow mid <> " variant " <> tshow var
        logDbg $ "From " <> tshow u
        req <- parseRequest u
        liftIO $ runConduitRes $ httpSource req getResponseBody .| sinkFile dest
        pure dest

      extractGPMD :: String -> FilePath -> GoPro BS.ByteString
      extractGPMD mid f = do
        ms <- liftIO $ findGPMDStream f
        case ms of
          Nothing -> logError ("Can't find GPMD stream for " <> tshow mid) >> empty
          Just s -> (liftIO $ extractGPMDStream f s)

runCleanup :: GoPro ()
runCleanup = do
  tok <- getToken
  ms <- filter wanted <$> listAll tok
  liftIO $ mapM_ (rm tok) ms

    where
      wanted Media{..} = _media_ready_to_view `elem` ["uploading", "failure"]
      rm tok Media{..} = do
        putStrLn $ "Removing " <> _media_id <> " (" <> _media_ready_to_view <> ")"
        errs <- delete tok _media_id
        when (not $ null errs) $ putStrLn $ " error: " <> show errs

runAuth :: GoPro ()
runAuth = do
  liftIO (prompt "Enter email: ")
  u <- liftIO getLine
  p <- liftIO getPass
  db <- asks dbConn
  res <- authenticate u p
  updateAuth db res

  where
    prompt x = putStr x >> hFlush stdout
    withEcho echo action = do
      prompt "Enter password: "
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    getPass = withEcho False getLine

runReauth :: GoPro ()
runReauth = do
  db <- asks dbConn
  a <- loadAuth db
  res <- refreshAuth a
  updateAuth db res

getToken :: (MonadLogger m, MonadIO m, MonadReader Env m) => m String
getToken = do
  logDbg "Loading token"
  loadToken =<< asks dbConn

runServer :: GoPro ()
runServer = ask >>= \x -> scottyT 8008 (runIO x) application
  where
    runIO :: Env -> EnvM a -> IO a
    runIO e m = runReaderT (runEnvM m) e

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
        gs <- selectGPMF db
        json $ map (\m@Media{..} ->
                      case Map.lookup _media_id gs of
                        Nothing -> m
                        Just g -> let cam = maybe (Just $ _cameraModel g) Just _media_camera_model in
                                    m & media_camera_model .~ cam & media_gpmf_data .~ Just g
                   ) ms

      get "/api/retrieve/:id" $ do
        imgid <- param "id"
        tok <- lift getToken
        setHeader "Content-Type" "application/json"
        raw =<< (lift $ proxy tok (dlURL imgid))

      get "/thumb/:id" $ do
        db <- lift $ asks dbConn
        imgid <- param "id"
        setHeader "Content-Type" "image/jpeg"
        setHeader "Cache-Control" "max-age=86400"
        raw =<< loadThumbnail db imgid

      get "/api/retrieve2/:id" $ do
        imgid <- param "id"
        tok <- lift getToken
        fi <- _fileStuff <$> retrieve tok imgid
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
run "auth"     = runAuth
run "reauth"   = runReauth
run "sync"     = runSync Incremental
run "fullsync" = runSync Full
run "cleanup"  = runCleanup
run "serve"    = runServer
run "getgpmf"  = runGetGPMF
run "groktel"  = runGrokTel
run x          = fail ("unknown command: " <> x)

main :: IO ()
main = do
  o@Options{..} <- execParser opts
  withConnection optDBPath (runConn o)

  where
    opts = info (options <**> helper)
           ( fullDesc <> progDesc "GoPro cloud utility.")

    runConn o@Options{..} db = do
      runStderrLoggingT . logfilt $ do
        l <- askLoggerIO
        runReaderT (run (head optArgv)) (Env o db l)

        where
          logfilt = filterLogger (\_ -> flip (if optVerbose then (>=) else (>)) LevelDebug)
