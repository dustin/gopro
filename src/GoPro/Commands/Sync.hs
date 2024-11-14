{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}

module GoPro.Commands.Sync where

import           Cleff
import           Cleff.Fail
import           Cleff.Reader
import           Conduit
import           Control.Applicative   (optional)
import           Control.Concurrent    (threadDelay)
import           Control.Lens
import           Control.Monad         (unless, void)
import           Control.Monad.Loops   (iterateWhile, whileM_)
import           Control.Retry         (RetryStatus (..), exponentialBackoff, limitRetries, recoverAll)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.Char             (toLower)
import           Data.Foldable         (fold, for_, traverse_)
import           Data.List             (partition)
import           Data.List.Extra       (chunksOf)
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (catMaybes, fromMaybe, isJust, maybeToList)
import qualified Data.Set              as Set
import qualified Data.Text             as T
import           Exif
import           FFMPeg
import           Graphics.HsExif       (parseExif)
import           Network.HTTP.Conduit
import           Network.HTTP.Simple   (getResponseBody, httpSource)
import           Network.Wreq          (head_, responseHeader)
import           System.Directory      (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import           System.FilePath.Posix (takeExtension, (</>))

import           Data.Functor          (($>))
import           UnliftIO.Async        (pooledMapConcurrentlyN, pooledMapConcurrentlyN_)
import           UnliftIO.Exception    (SomeException, try)

import           GoPro.Alternative
import           GoPro.AuthCache
import           GoPro.Commands
import           GoPro.Commands.Backup (runStoreMeta')
import           GoPro.Config.Effect
import           GoPro.DB
import           GoPro.Logging
import           GoPro.Meta
import           GoPro.Plus.Media
import           GoPro.Resolve
import           GoPro.S3

data SyncType = Full
    | Incremental

runFetch :: [Reader Options, AuthCache, LogFX, DB, Fail, IOE] :>> es => SyncType -> Eff es ()
runFetch stype = do
  seen <- Set.fromList <$> loadMediaIDs
  (ms, unwanted) <- todo seen
  traverse_ logUnwanted unwanted
  logInfoL [tshow (length ms), " new items"]
  unless (null ms) $ logDbgL ["new items: ", tshow (ms ^.. folded . medium_id)]
  mapM_ storeSome $ chunksOf 100 ms

    where resolve m = recoverAll policy $ \rt -> do
            unless (rsIterNumber rt == 0) $ logInfoL ["Retrying fetch of ", _medium_id m,
                                                      " attempt ", tshow (rsIterNumber rt)]
            MediaRow m' _ _ r <- medium (_medium_id m)
            variants <- fetchVariantsSansURLs (_medium_id m)
            thumbs <- runTryAlternative . optional $ TryAlternative (fetchThumbnail m)
            pure $ MediaRow m' thumbs variants r

          policy = exponentialBackoff 2000000 <> limitRetries 3

          todo seen = partition wanted . filter notSeen <$> listWhile (listPred stype)
            where
              notSeen = (`Set.notMember` seen) . _medium_id
              listPred Incremental = all notSeen
              listPred Full        = const True
              wanted Medium{..} = isJust _medium_file_size && _medium_ready_to_view == ViewReady

          storeSome l = do
            logInfoL ["Storing batch of ", tshow (length l)]
            c <- asksOpt optDownloadConcurrency
            storeMedia =<< fetch c l
          fetch c = pooledMapConcurrentlyN c resolve

          logUnwanted Medium{..} = logDbgL ["Skipping ", _medium_id,
                                            " fn=", maybe "[none]" T.pack _medium_filename,
                                            ", view state: ", tshow _medium_ready_to_view,
                                            ", size: ", tshow _medium_file_size,
                                            ", see: https://gopro.com/media-library/", _medium_id , "/"]

removeDeleted :: [Reader Options, AuthCache, LogFX, DB, Fail, IOE] :>> es => Eff es ()
removeDeleted = do
  loc <- loadMediaIDs
  logDbgL ["Found ", tshow (length loc), " local items"]
  remote <- loadRemoteIDs
  logDbgL ["Found ", tshow (length remote), " remote items"]
  let missing = Set.difference (Set.fromList loc) (Set.fromList remote)
  unless (null missing) do
     logInfoL ["Deleting missing items: ", tshow missing]
     deleteMedia (Set.toList missing)

  where
    loadRemoteIDs = toListOf (folded . medium_id) <$> listAll

runGetMoments :: [Reader Options, AuthCache, LogFX, DB, IOE] :>> es => Eff es ()
runGetMoments = do
  need <- momentsTODO
  unless (null need) $ logInfoL ["Need to fetch ", (tshow . length) need, " moments"]
  c <- asksOpt optDownloadConcurrency
  mapM_ (uncurry storeMoments) =<< pooledMapConcurrentlyN c pickup need
    where pickup mid = (mid,) <$> moments mid

runGrokTel :: [LogFX, DB, IOE] :>> es => Eff es [(MediumID, Maybe BS.ByteString)]
runGrokTel = fmap catMaybes . traverse ud =<< metaTODO
    where
      ud (mid, typ, bs) = do
        logInfoL ["Updating ", tshow (mid, typ)]
        case summarize typ bs of
          Left x  -> logErrorL ["Error parsing stuff for ", tshow mid, " show ", tshow x] $> Nothing
          Right x -> insertMeta mid x $> Just (mid, Just bs)
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

runGetMeta :: forall es. [Reader Options, AuthCache, LogFX, S3, DB, Fail, IOE] :>> es => Eff es ()
runGetMeta = do
  needs <- metaBlobTODO
  logInfoL ["Fetching meta ", tshow (length needs)]
  logDbgL ["Need meta: ", tshow needs]
  c <- asksOpt optDownloadConcurrency
  pooledMapConcurrentlyN_ c process needs
    where
      process :: (MediumID, String) -> Eff es ()
      process mtyp@(mid,typ) = do
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
              let fv :: String -> String -> FilePath -> Eff es (Maybe BS.ByteString)
                  fv u v p = Just <$> fetchX ex mid v u p
                  fn v = ".cache" </> T.unpack mid <> "-" <> v <> fx
                  candidates = [ fv u l (fn l) | (u,l) <- metadataSources fi]
              ms <- tsum $ (Just . BL.toStrict <$> getMetaBlob mid) : candidates <> [pure Nothing]
              case ms of
                Nothing -> do
                  logInfoL ["Found no metadata for ", tshow mtyp]
                  insertMetaBlob mid NoMetadata Nothing
                Just s -> do
                  logInfoL ["MetaData stream for ", tshow mtyp, " is ", tshow (BS.length s), " bytes"]
                  insertMetaBlob mid fmt (Just s)

              -- Always clean up (should make this optional at some point).
              mapM_ ((\f -> tsum [liftIO (removeFile f), pure ()]) . fn . snd) (metadataSources fi)

      fetchX :: (MediumID -> FilePath -> Eff es BS.ByteString)
             -> MediumID -> String -> String -> FilePath -> Eff es BS.ByteString
      fetchX ex mid var u fn = ex mid =<< dlIf mid var u fn

      dlIf :: MediumID -> String -> String -> FilePath -> Eff es FilePath
      dlIf mid var u dest = liftIO (doesFileExist dest) >>=
        \case
          True -> pure dest
          _    -> download mid var u dest

      download :: MediumID -> String -> String -> FilePath -> Eff es FilePath
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

      extractEXIF :: MediumID -> FilePath -> Eff es BS.ByteString
      extractEXIF mid f = liftIO (BL.readFile f) >>= (\case
          Left s  -> logErrorL ["Can't find EXIF for ", tshow mid, " ", tshow s] *> fail "no exif"
          Right e -> pure (BL.toStrict e)) . minimalEXIF

      extractGPMD :: MediumID -> FilePath -> Eff es BS.ByteString
      extractGPMD mid f = liftIO (findGPMDStream f) >>=
        \case
          Nothing -> logErrorL ["Can't find GPMD stream for ", tshow mid] *> fail "no gpmd"
          Just s  -> liftIO $ extractGPMDStream [f] s

runWait :: [AuthCache, LogFX, IOE] :>> es => Eff es ()
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

refreshMedia :: [Reader Options, AuthCache, LogFX, DB, Fail, IOE] :>> es => NonEmpty MediumID -> Eff es ()
refreshMedia = mapM_ refreshSome . chunksOf 100 . NE.toList
  where
    one mid = do
      logDbgL ["Refreshing ", mid]
      MediaRow m _ _ r <- medium mid
      tn <- either (\(_ :: SomeException) -> Nothing) Just <$> try (fetchThumbnail m)
      variants <- fetchVariantsSansURLs (_medium_id m)
      pure $ MediaRow m tn variants r

    refreshSome mids = do
      c <- asksOpt optDownloadConcurrency
      logInfoL ["Processing batch of ", tshow (length mids)]
      n <- pooledMapConcurrentlyN c one mids
      logDbgL ["Storing ", tshow (length n)]
      storeMedia n

findGPSReadings :: [LogFX, DB, Fail] :>> es => Eff es ()
findGPSReadings = do
  todo <- gPSReadingsTODO
  unless (null todo) $ logInfoL ["Found ", tshow (length todo), " gps readings to process"]
  for_ todo $ \mid -> do
    logDbgL ["Finding gps readings for ", mid]
    loadMetaBlob mid >>= \case
      Just (GPMF, Just bs) -> storeGPSReadings mid =<< either fail pure (extractReadings bs)
      _                    -> pure ()

syncFiles :: [Reader Options, AuthCache, LogFX, DB, IOE] :>> es => Eff es ()
syncFiles = do
  todo <- fileTODO
  unless (null todo) $ do
    logInfoL ["Found ", tshow (length todo), " media with files to process"]
    traverse_ process todo

    where
      process mid = do
        stuff <- extractFiles mid <$> retrieve mid
        if null stuff
        then logInfoL ["No files found for ", mid]
        else do
          logDbgL ["Found ", tshow (length stuff), " files for ", mid]
          c <- asksOpt optDownloadConcurrency
          storeFiles . catMaybes =<< pooledMapConcurrentlyN c updateLength stuff

      updateLength (hu, _, f) = do
        r <- try @_ @SomeException $ liftIO (head_ hu)
        case r ^? _Right . responseHeader "Content-Length" of
          Nothing  -> logErrorL ["Could not find data for ", tshow f, tshow r] $> Nothing
          Just len -> pure (Just f{_fd_file_size = read (BC.unpack len)})

extractFiles :: MediumID -> FileInfo -> [(String, String, FileData)]
extractFiles mid fi = fold [ ex "var" variations,
                             ex2 "sidecar" sidecar_files,
                             otherFiles
                           ]
  where
    ex p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          pure (v ^. media_head, v ^. media_url, FileData{
            _fd_medium = mid,
            _fd_section = p,
            _fd_label = T.pack lbl,
            _fd_type = T.pack typ,
            _fd_item_num = fromMaybe 0 (v ^. media_item_number),
            _fd_file_size = 0 -- Filled in before being stored
            })
    -- This is a copy of the above due to some monomorphism issue I don't want to deal with yet.  TODO or whatever.
    ex2 p l = fi ^.. fileStuff . l . folded . to conv . folded
      where
        conv v = maybeToList $ do
          lbl <- v ^? media_label
          typ <- v ^? media_type
          pure (v ^. media_head, v ^. media_url, FileData{
            _fd_medium = mid,
            _fd_section = p,
            _fd_label = T.pack lbl,
            _fd_type = T.pack typ,
            _fd_item_num = fromMaybe 0 (v ^. media_item_number),
            _fd_file_size = 0 -- Filled in before being stored
            })

    otherFiles = fi ^.. fileStuff . files . folded . to conv
      where
        typ = fi ^. filename . to (fmap toLower . drop 1 . takeExtension)
        conv v = let i = v ^. file_item_number
                     lbl = show i <> "." <> typ
                 in (v ^. media_head, v ^. media_url, FileData{
                      _fd_medium = mid,
                      _fd_section = "files",
                      _fd_label = T.pack lbl,
                      _fd_type = T.pack typ,
                      _fd_item_num = fromMaybe 0 (v ^. media_item_number),
                      _fd_file_size = 0 -- TBD
                    })

runFullSync :: [Reader Options, ConfigFX, AuthCache, LogFX, S3, DB, Fail, IOE] :>> es => Eff es ()
runFullSync = do
  runFetch Incremental
  runGetMeta
  void . iterateWhile (not . null) $ do
    metas <- runGrokTel
    unless (null metas) $ logDbgL ["Did a batch of ", tshow (length metas)]
    -- If an S3 bucket is configured, make sure all metadata is in the cache.
    bn <- configItem CfgBucket
    unless (bn == "") $ runStoreMeta' metas
    pure metas
  runGetMoments
  syncFiles
  findGPSReadings
