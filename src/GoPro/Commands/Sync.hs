{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module GoPro.Commands.Sync where

import           Conduit
import           Control.Applicative    (Alternative (..))
import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad          (unless)
import           Control.Monad.Loops    (whileM_)
import qualified Data.List.NonEmpty     as NE
import           Data.List.NonEmpty     (NonEmpty(..))
import           Control.Monad.Reader   (asks)
import           Control.Retry          (RetryStatus (..), exponentialBackoff, limitRetries, recoverAll)
import qualified Data.Aeson             as J
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
import           Network.HTTP.Simple    (getResponseBody, httpSource, parseRequest)
import           System.Directory       (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
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
  logInfoL [tshow (length ms), " new items"]
  unless (null ms) $ logDbgL ["new items: ", tshow (ms ^.. folded . medium_id)]
  mapM_ storeSome $ chunksOf 100 ms

    where resolve m = MediaRow m <$> fetchThumbnail m <*> (J.encode <$> fetchVariantsSansURLs (_medium_id m))
          todo seen = filter (\m -> notSeen m && wanted m) <$> listWhile (listPred stype)
            where
              notSeen = (`Set.notMember` seen) . _medium_id
              listPred Incremental = all notSeen
              listPred Full        = const True
              wanted Medium{..} = isJust _medium_file_size && _medium_ready_to_view == ViewReady

          storeSome l = do
            logInfoL ["Storing batch of ", tshow (length l)]
            c <- asks (optDownloadConcurrency . gpOptions)
            storeMedia =<< fetch c l
          fetch c = mapConcurrentlyLimited c resolve

runGetMoments :: GoPro ()
runGetMoments = do
  need <- momentsTODO
  unless (null need) $ logInfoL ["Need to fetch ", (tshow . length) need, " moments"]
  c <- asks (optDownloadConcurrency . gpOptions)
  mapM_ (uncurry storeMoments) =<< mapConcurrentlyLimited c pickup need
    where pickup mid = (mid,) <$> moments mid

runGrokTel :: GoPro ()
runGrokTel = mapM_ ud =<< metaTODO
    where
      ud (mid, typ, bs) = do
        logInfoL ["Updating ", tshow (mid, typ)]
        case summarize typ bs of
          Left x  -> logErrorL ["Error parsing stuff for ", tshow mid, " show ", tshow x]
          Right x -> insertMeta mid x
      summarize :: MetadataType -> BS.ByteString -> Either String MDSummary
      summarize GPMF bs      = summarizeGPMF <$> parseDEVC bs
      summarize EXIF bs      = summarizeEXIF <$> parseExif (BL.fromStrict bs)
      summarize NoMetadata _ = Left "Can't summarize with no metadata"

runGetMeta :: GoPro ()
runGetMeta = do
  needs <- metaBlobTODO
  logInfoL ["Fetching meta ", tshow (length needs)]
  logDbgL ["Need meta: ", tshow needs]
  mapM_ process needs
    where
      process :: (MediumID, String) -> GoPro ()
      process mtyp@(mid,typ) = do
        fi <- retrieve mid
        case typ of
          "Video"          -> processEx fi extractGPMD GPMF ".mp4"
          "TimeLapseVideo" -> processEx fi extractGPMD GPMF ".mp4"
          "Photo"          -> processEx fi extractEXIF EXIF ".jpg"
          "TimeLapse"      -> processEx fi extractEXIF EXIF ".jpg"
          "Burst"          -> processEx fi extractEXIF EXIF ".jpg"
          x                -> logErrorL ["Unhandled type: ", tshow x]

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
                  logInfoL ["Found no metadata for ", tshow mtyp]
                  insertMetaBlob mid NoMetadata Nothing
                Just s -> do
                  logInfoL ["MetaData stream for ", tshow mtyp, " is ", tshow (BS.length s), " bytes"]
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
          _    -> download mid var u dest

      download :: MediumID -> String -> String -> FilePath -> GoPro FilePath
      download mid var u dest = recoverAll policy $ \r -> do
        liftIO $ createDirectoryIfMissing True ".cache"
        logInfoL ["Fetching ", tshow mid, " variant ", tshow var, " attempt ", tshow (rsIterNumber r)]
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
          Left s  -> logErrorL ["Can't find EXIF for ", tshow mid, tshow s] >> empty
          Right e -> pure (BL.toStrict e)

      extractGPMD :: MediumID -> FilePath -> GoPro BS.ByteString
      extractGPMD mid f = do
        ms <- liftIO $ findGPMDStream f
        case ms of
          Nothing -> logErrorL ["Can't find GPMD stream for ", tshow mid] >> empty
          Just s  -> liftIO $ extractGPMDStream f s

runWaitForUploads :: GoPro ()
runWaitForUploads = whileM_ inProgress (sleep 15)
  where
    inProgress = do
      ms <- toListOf (folded . filtered ((`elem` [ViewUploading, ViewTranscoding]) . view medium_ready_to_view)) . fst <$> list 10 1
      let ups = filter (when ViewUploading) ms
          ts = filter (when ViewTranscoding) ms
      unless (null ups) $ logInfoL ["Still uploading: ", tshow (ids ups)]
      unless (null ts) $ logInfoL ["Still transcoding: ", tshow (ids ts)]
      pure $ (not.null) (ups <> ts)

    ids = toListOf (folded . medium_id)
    when x Medium{_medium_ready_to_view} = x == _medium_ready_to_view
    sleep = liftIO . threadDelay . seconds
    seconds = (* 1000000)

refreshMedia :: NonEmpty MediumID -> GoPro ()
refreshMedia = mapM_ refreshSome . chunksOf 100 . NE.toList
  where
    one mid = do
      logDbgL ["Refreshing ", mid]
      MediaRow <$> medium mid <*> pure mempty <*> (J.encode <$> fetchVariantsSansURLs mid)

    refreshSome mids = do
      c <- asks (optDownloadConcurrency . gpOptions)
      logInfoL ["Processing batch of ", tshow (length mids)]
      n <- mapConcurrentlyLimited c one mids
      logDbgL ["Storing ", tshow (length n)]
      storeMedia n

runFullSync :: GoPro ()
runFullSync = do
  runFetch Incremental
  runGetMeta
  runGrokTel
  runGetMoments
  -- If an S3 bucket is configured, make sure all metadata is in the cache.
  bn <- asks (configItem CfgBucket)
  unless (bn == "") runStoreMeta
