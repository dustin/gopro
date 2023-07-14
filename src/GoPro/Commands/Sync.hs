{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}

module GoPro.Commands.Sync where

import           Conduit
import           Control.Applicative   (Alternative (..), optional)
import           Control.Concurrent    (threadDelay)
import           Control.Lens
import           Control.Monad         (unless, void)
import           Control.Monad.Loops   (iterateWhile, whileM_)
import           Control.Monad.Reader  (asks)
import           Control.Retry         (RetryStatus (..), exponentialBackoff, limitRetries, recoverAll)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.Foldable         (asum, fold, for_, traverse_)
import           Data.List.Extra       (chunksOf)
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (catMaybes, isJust)
import qualified Data.Set              as Set
import qualified Data.Text             as T
import           Exif
import           FFMPeg
import           Graphics.HsExif       (parseExif)
import           Network.HTTP.Simple   (getResponseBody, httpSource, parseRequest)
import           System.Directory      (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import           System.FilePath.Posix ((</>))

import           Data.Functor          (($>))
import           GoPro.Commands
import           GoPro.Commands.Backup (runStoreMeta')
import           GoPro.DB
import           GoPro.Meta
import           GoPro.Plus.Media
import           GoPro.Resolve
import           GoPro.S3
import           UnliftIO.Async        (pooledMapConcurrentlyN, pooledMapConcurrentlyN_)


data SyncType = Full
    | Incremental

runFetch :: SyncType -> GoPro ()
runFetch stype = do
  db <- asks database
  seen <- Set.fromList <$> loadMediaIDs db
  ms <- todo seen
  logInfoL [tshow (length ms), " new items"]
  unless (null ms) $ logDbgL ["new items: ", tshow (ms ^.. folded . medium_id)]
  mapM_ (storeSome db) $ chunksOf 100 ms

    where resolve m = recoverAll policy $ \rt -> do
            unless (rsIterNumber rt == 0) $ logInfoL ["Retrying fetch of ", _medium_id m,
                                                      " attempt ", tshow (rsIterNumber rt)]
            MediaRow m' _ _ r <- medium (_medium_id m)
            Just variants <- fetchVariantsSansURLs (_medium_id m) -- TODO:  Maybe make this total
            thumbs <- optional (fetchThumbnail m)
            pure $ MediaRow m' thumbs variants r

          policy = exponentialBackoff 2000000 <> limitRetries 3

          todo seen = filter (\m -> notSeen m && wanted m) <$> listWhile (listPred stype)
            where
              notSeen = (`Set.notMember` seen) . _medium_id
              listPred Incremental = all notSeen
              listPred Full        = const True
              wanted Medium{..} = isJust _medium_file_size && _medium_ready_to_view == ViewReady

          storeSome db l = do
            logInfoL ["Storing batch of ", tshow (length l)]
            c <- asksOpt optDownloadConcurrency
            storeMedia db =<< fetch c l
          fetch c = pooledMapConcurrentlyN c resolve

runGetMoments :: GoPro ()
runGetMoments = do
  Database{..} <- asks database
  need <- momentsTODO
  unless (null need) $ logInfoL ["Need to fetch ", (tshow . length) need, " moments"]
  c <- asksOpt optDownloadConcurrency
  mapM_ (uncurry storeMoments) =<< pooledMapConcurrentlyN c pickup need
    where pickup mid = (mid,) <$> moments mid

runGrokTel :: GoPro [(MediumID, Maybe BS.ByteString)]
runGrokTel = asks database >>= \db -> fmap catMaybes . traverse (ud db) =<< metaTODO db
    where
      ud db (mid, typ, bs) = do
        logInfoL ["Updating ", tshow (mid, typ)]
        case summarize typ bs of
          Left x  -> logErrorL ["Error parsing stuff for ", tshow mid, " show ", tshow x] $> Nothing
          Right x -> insertMeta db mid x $> Just (mid, Just bs)
      summarize :: MetadataType -> BS.ByteString -> Either String MDSummary
      summarize GPMF bs      = summarizeGPMF <$> parseDEVC bs
      summarize EXIF bs      = summarizeEXIF <$> parseExif (BL.fromStrict bs)
      summarize NoMetadata _ = Left "Can't summarize with no metadata"

-- | extract a list of metadata source candidates.
metadataSources :: FileInfo -> [(String, String)]
metadataSources fi = fold [variation "mp4_low" "low",
                           variation "high_res_proxy_mp4" "high",
                           variation "concat" "concat",
                           variation "source" "src",
                           variation "baked_source" "baked_src"]
  where
    ls l t = (,t) <$> toListOf l fi

    variation var = ls (fileStuff . variations . folded . filtered (has (var_label . only var)) . var_url)

runGetMeta :: GoPro ()
runGetMeta = do
  db <- asks database
  needs <- metaBlobTODO db
  logInfoL ["Fetching meta ", tshow (length needs)]
  logDbgL ["Need meta: ", tshow needs]
  c <- asksOpt optDownloadConcurrency
  pooledMapConcurrentlyN_ c (process db) needs
    where
      process :: Database -> (MediumID, String) -> GoPro ()
      process db mtyp@(mid,typ) = do
        fi <- retrieve mid
        case typ of
          "Video"          -> processEx fi extractGPMD GPMF ".mp4"
          "MultiClipEdit"  -> processEx fi extractGPMD GPMF ".mp4"
          "TimeLapseVideo" -> processEx fi extractGPMD GPMF ".mp4"
          "Photo"          -> processEx fi extractEXIF EXIF ".jpg"
          "TimeLapse"      -> processEx fi extractEXIF EXIF ".jpg"
          "Burst"          -> processEx fi extractEXIF EXIF ".jpg"
          x                -> logErrorL ["Unhandled type: ", tshow x]

          where
            processEx fi ex fmt fx = do
              let fv :: String -> String -> FilePath -> GoPro (Maybe BS.ByteString)
                  fv u v p = Just <$> fetchX ex mid v u p
                  fn v = ".cache" </> T.unpack mid <> "-" <> v <> fx
                  candidates = [ fv u l (fn l) | (u,l) <- metadataSources fi]
              ms <- asum $ (Just . BL.toStrict <$> getMetaBlob mid) : candidates <> [pure Nothing]
              case ms of
                Nothing -> do
                  logInfoL ["Found no metadata for ", tshow mtyp]
                  insertMetaBlob db mid NoMetadata Nothing
                Just s -> do
                  logInfoL ["MetaData stream for ", tshow mtyp, " is ", tshow (BS.length s), " bytes"]
                  insertMetaBlob db mid fmt (Just s)

              -- Always clean up (should make this optional at some point).
              mapM_ ((\f -> asum [liftIO (removeFile f), pure ()]) . fn . snd) (metadataSources fi)

      fetchX :: (MediumID -> FilePath -> GoPro BS.ByteString)
             -> MediumID -> String -> String -> FilePath -> GoPro BS.ByteString
      fetchX ex mid var u fn = ex mid =<< dlIf mid var u fn

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
      extractEXIF mid f = liftIO (BL.readFile f) >>= (\case
          Left s  -> logErrorL ["Can't find EXIF for ", tshow mid, " ", tshow s] *> empty
          Right e -> pure (BL.toStrict e)) . minimalEXIF

      extractGPMD :: MediumID -> FilePath -> GoPro BS.ByteString
      extractGPMD mid f = liftIO (findGPMDStream f) >>=
        \case
          Nothing -> logErrorL ["Can't find GPMD stream for ", tshow mid] >> empty
          Just s  -> liftIO $ extractGPMDStream [f] s

runWait :: GoPro ()
runWait = whileM_ inProgress (sleep 15)
  where
    inProgress = do
      ms <- recoverAll policy $ \r -> do
        unless (rsIterNumber r == 0) $ logInfoL ["Retrying notReady call attempt ", tshow (rsIterNumber r)]
        notReady
      let breakdown = Map.fromListWith (<>) [(i ^. medium_ready_to_view . to show,
                                              [(i ^. medium_id, i ^. medium_filename . _Just)]) | i <- ms]
      traverse_ (liftIO . display) (Map.assocs breakdown)
      pure $ (not.null) (filter (not . when ViewFailure) ms)

    policy = exponentialBackoff 2000000 <> limitRetries 9

    when x Medium{_medium_ready_to_view} = x == _medium_ready_to_view
    sleep = liftIO . threadDelay . seconds
    seconds = (* 1000000)

    display (t, things) = do
      putStrLn $ fold [drop 4 t, ":"]
      traverse_ (\(i, fn) -> putStrLn $ fold ["  ", T.unpack i, " - ", show fn]) things

refreshMedia :: NonEmpty MediumID -> GoPro ()
refreshMedia = mapM_ refreshSome . chunksOf 100 . NE.toList
  where
    one mid = do
      logDbgL ["Refreshing ", mid]
      MediaRow m _ _ r <- medium mid
      tn <- optional (fetchThumbnail m)
      Just variants <- fetchVariantsSansURLs (_medium_id m) -- TODO:  Maybe make this total
      pure $ MediaRow m tn variants r

    refreshSome mids = do
      Database{..} <- asks database
      c <- asksOpt optDownloadConcurrency
      logInfoL ["Processing batch of ", tshow (length mids)]
      n <- pooledMapConcurrentlyN c one mids
      logDbgL ["Storing ", tshow (length n)]
      storeMedia n

findGPSReadings :: GoPro ()
findGPSReadings = do
  Database{..} <- asks database
  todo <- gpsReadingsTODO
  unless (null todo) $ logInfoL ["Found ", tshow (length todo), " gps readings to process"]
  for_ todo $ \mid -> do
    logDbgL ["Finding gps readings for ", mid]
    loadMetaBlob mid >>= \case
      Just (GPMF, Just bs) -> storeGPSReadings mid =<< either fail pure (extractReadings bs)
      _                    -> pure ()

runFullSync :: GoPro ()
runFullSync = do
  runFetch Incremental
  runGetMeta
  void . iterateWhile (not . null) $ do
    metas <- runGrokTel
    unless (null metas) $ logDbgL ["Did a batch of ", tshow (length metas)]
    -- If an S3 bucket is configured, make sure all metadata is in the cache.
    bn <- asks (configItem CfgBucket)
    unless (bn == "") $ runStoreMeta' metas
    pure metas
  runGetMoments
  findGPSReadings
