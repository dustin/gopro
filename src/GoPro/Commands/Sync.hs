{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module GoPro.Commands.Sync where

import           Conduit
import           Control.Applicative    (Alternative (..))
import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Loops    (whileM_)
import           Control.Monad.Reader   (asks)
import           Control.Retry          (RetryStatus (..), exponentialBackoff,
                                         limitRetries, recoverAll)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable          (asum)
import           Data.List.Extra        (chunksOf)
import           Data.Maybe             (isJust)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           Exif
import           FFMPeg
import           Graphics.HsExif        (parseExif)
import           Network.HTTP.Simple    (getResponseBody, httpSource,
                                         parseRequest)
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist, removeFile, renameFile)
import           System.FilePath.Posix  ((</>))

import           GoPro.Commands
import           GoPro.Commands.Backup  (runStoreMeta)
import           GoPro.DB
import           GoPro.Plus.Media
import           GoPro.Resolve
import           GoPro.S3


data SyncType = Full
    | Incremental

runFetch :: SyncType -> GoPro ()
runFetch stype = do
  seen <- Set.fromList <$> loadMediaIDs
  ms <- todo seen
  logInfo $ tshow (length ms) <> " new items"
  unless (null ms) $ logDbg $ "new items: " <> tshow (ms ^.. folded . medium_id)
  mapM_ storeSome $ chunksOf 100 ms

    where resolve m = MediaRow m <$> fetchThumbnail m
          todo seen = filter (\m -> notSeen m && wanted m) <$> listWhile (listPred stype)
            where
              notSeen = (`Set.notMember` seen) . _medium_id
              listPred Incremental = all notSeen
              listPred Full        = const True
              wanted Medium{..} = isJust _medium_file_size && _medium_ready_to_view == ViewReady
          storeSome l = do
            logInfo $ "Storing batch of " <> tshow (length l)
            c <- asks (optDownloadConcurrency . gpOptions)
            storeMedia =<< fetch c l
          fetch c = mapConcurrentlyLimited c resolve

runGetMoments :: GoPro ()
runGetMoments = do
  need <- momentsTODO
  unless (null need) $ logInfo ("Need to fetch " <> (tshow . length) need <> " moments")
  c <- asks (optDownloadConcurrency . gpOptions)
  mapM_ (uncurry storeMoments) =<< mapConcurrentlyLimited c pickup need
    where pickup mid = (mid,) <$> moments mid

runGrokTel :: GoPro ()
runGrokTel = mapM_ ud =<< metaTODO
    where
      ud (mid, typ, bs) = do
        logInfo $ "Updating " <> tshow (mid, typ)
        case summarize typ bs of
          Left x -> logError $ "Error parsing stuff for " <> tshow mid <> " show " <> tshow x
          Right x -> insertMeta mid x
      summarize :: String -> BS.ByteString -> Either String MDSummary
      summarize "gpmf" bs = summarizeGPMF <$> parseDEVC bs
      summarize "exif" bs = summarizeEXIF <$> parseExif (BL.fromStrict bs)
      summarize fmt _     = Left ("Can't summarize " <> show fmt)

runGetMeta :: GoPro ()
runGetMeta = do
  needs <- metaBlobTODO
  logInfo $ "Fetching meta " <> tshow (length needs)
  logDbg $ "Need meta: " <> tshow needs
  mapM_ process needs
    where
      process :: (MediumID, String) -> GoPro ()
      process mtyp@(mid,typ) = do
        fi <- retrieve mid
        case typ of
          "Video"          -> processEx fi extractGPMD "gpmf" ".mp4"
          "TimeLapseVideo" -> processEx fi extractGPMD "gpmf" ".mp4"
          "Photo"          -> processEx fi extractEXIF "exif" ".jpg"
          "TimeLapse"      -> processEx fi extractEXIF "exif" ".jpg"
          "Burst"          -> processEx fi extractEXIF "exif" ".jpg"
          x                -> logError $ "Unhandled type: " <> tshow x

          where
            processEx fi ex fmt fx = do
              let fv :: String -> FilePath -> GoPro (Maybe BS.ByteString)
                  fv s p = Just <$> fetchX ex fi mid s p
                  fn v = ".cache" </> T.unpack mid <> "-" <> v <> fx
              ms <- asum [
                Just . BL.toStrict <$> getMetaBlob mid,
                fv "mp4_low" (fn "low"),
                fv "high_res_proxy_mp4" (fn "high"),
                fv "source" (fn "src"),
                pure Nothing]
              case ms of
                Nothing -> do
                  logInfo $ "Found no metadata for " <> tshow mtyp
                  insertMetaBlob mid "" Nothing
                Just s -> do
                  logInfo $ "MetaData stream for " <> tshow mtyp <> " is " <> tshow (BS.length s) <> " bytes"
                  insertMetaBlob mid fmt (Just s)
                  -- Clean up in the success case.
                  mapM_ ((\f -> asum [liftIO (removeFile f), pure ()]) . fn) ["low", "high", "src"]

      fetchX :: (MediumID -> FilePath -> GoPro BS.ByteString)
             -> FileInfo -> MediumID -> String -> FilePath -> GoPro BS.ByteString
      fetchX ex fi mid var fn = do
        let mu = fi ^? fileStuff . variations . folded . filtered (has (var_label . only var)) . var_url
        case mu of
          Nothing -> empty
          Just u  -> ex mid =<< dlIf mid var u fn

      dlIf :: MediumID -> String -> String -> FilePath -> GoPro FilePath
      dlIf mid var u dest = liftIO (doesFileExist dest) >>=
        \case
          True -> pure dest
          _ -> download mid var u dest

      download :: MediumID -> String -> String -> FilePath -> GoPro FilePath
      download mid var u dest = recoverAll policy $ \r -> do
        liftIO $ createDirectoryIfMissing True ".cache"
        logInfo $ "Fetching " <> tshow mid <> " variant " <> tshow var <> " attempt " <> tshow (rsIterNumber r)
        req <- parseRequest u
        let tmpfile = dest <> ".tmp"
        liftIO $ runConduitRes (httpSource req getResponseBody .| sinkFile tmpfile) >>
                  renameFile tmpfile dest
        pure dest

          where
            policy = exponentialBackoff 2000000 <> limitRetries 9

      extractEXIF :: MediumID -> FilePath -> GoPro BS.ByteString
      extractEXIF mid f = do
        bs <- liftIO $ BL.readFile f
        case minimalEXIF bs of
          Left s -> logError ("Can't find EXIF for " <> tshow mid <> tshow s) >> empty
          Right e -> pure (BL.toStrict e)

      extractGPMD :: MediumID -> FilePath -> GoPro BS.ByteString
      extractGPMD mid f = do
        ms <- liftIO $ findGPMDStream f
        case ms of
          Nothing -> logError ("Can't find GPMD stream for " <> tshow mid) >> empty
          Just s -> liftIO $ extractGPMDStream f s

runWaitForUploads :: GoPro ()
runWaitForUploads = whileM_ inProgress (sleep 15)
  where
    inProgress = do
      ms <- toListOf (folded . filtered ((`elem` [ViewUploading, ViewTranscoding]) . view medium_ready_to_view)) . fst <$> list 10 1
      let ups = filter (when ViewUploading) ms
          ts = filter (when ViewTranscoding) ms
      unless (null ups) $ logInfo $ "Still uploading: " <> tshow (ids ups)
      unless (null ts) $ logInfo $ "Still transcoding: " <> tshow (ids ts)
      pure $ (not.null) (ups <> ts)

    ids = toListOf (folded . medium_id)
    when x Medium{_medium_ready_to_view} = x == _medium_ready_to_view
    sleep = liftIO . threadDelay . seconds
    seconds = (* 1000000)

runFullSync :: GoPro ()
runFullSync = do
  runWaitForUploads
  runFetch Incremental
  runGetMeta
  runGrokTel
  runGetMoments
  runStoreMeta
