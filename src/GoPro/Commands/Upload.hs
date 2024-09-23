{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module GoPro.Commands.Upload (
  runCreateUploads, runCreateMultipart, runResumeUpload, runReprocessCmd
  ) where

import           Cleff
import           Cleff.Reader              hiding (asks)
import           Control.Monad             (unless, void)
import           Control.Monad.Reader      (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BL
import           Data.Foldable             (asum, fold)
import           Data.List                 (sortOn)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Monoid               (Sum (..))
import           Data.Ord                  (Down (..))
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Data.These                (These (..), these)
import           System.Posix.Files        (fileSize, getFileStatus, isDirectory)

import           Exif
import           FFMPeg
import           GoPro.Commands
import           GoPro.DB
import           GoPro.File
import           GoPro.Logging
import           GoPro.Plus.Media          (MediumID, MediumType (..), reprocess)
import           GoPro.Plus.Upload
import           UnliftIO.Async            (pooledMapConcurrentlyN, pooledMapConcurrentlyN_)

uc :: ([LogFX, DatabaseEff, IOE] :>> es) => FilePath -> MediumID -> Integer -> UploadPart -> Uploader (Eff es) ()
uc fp mid partnum up@UploadPart{..} = do
  lift $ logDbgL ["Uploading part ", tshow _uploadPart, " of ", T.pack fp]
  uploadChunk fp up
  lift $ completedUploadPart mid _uploadPart partnum
  lift $ logDbgL ["Finished part ", tshow _uploadPart, " of ", T.pack fp]

runCreateUploads :: ([Reader Env, LogFX, DatabaseEff, IOE] :>> es) => NonEmpty FilePath -> Eff es ()
runCreateUploads inFilePaths = do
  filePaths <- Set.toList . fold <$> traverse expand (NE.toList inFilePaths)
  -- Exclude any commandline params for files that are already being
  -- uploaded.  This prevents duplicate uploads if you just hit
  -- up-enter, but it also prevents one from uploading a file if it's
  -- already included in a multipart upload.
  queued <- listQueuedFiles
  let candidates = filter (`notElem` queued) filePaths
      (bad, good) = these (,[]) ([],) (,) $ maybe (This []) parseAndGroup (NE.nonEmpty candidates)

  unless (null bad) $ logInfoL ["Ignoring some unknown files: ", tshow bad]

  c <- asksOpt optUploadConcurrency
  pooledMapConcurrentlyN_ c upload good

  where
    expand fp = liftIO (isDir fp) >>= \case
      True  -> findFiles fp
      False -> pure (Set.singleton fp)
    isDir = fmap isDirectory . getFileStatus
    upload files = asksOpt optChunkSize >>= \chunkSize -> runUpload (_gpFilePath <$> files) $ do
      setMediumType (typeOf (NE.head files))
      setChunkSize chunkSize
      mid <- createMedium
      did <- createSource (length files)
      c <- lift $ asksOpt optUploadConcurrency
      pooledMapConcurrentlyN_ c (\(File{_gpFilePath},n) -> do
                                    fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) _gpFilePath
                                    up@Upload{..} <- createUpload did n (fromInteger fsize)
                                    lift $ logInfoL ["Creating part ", tshow _gpFilePath, " as ", mid, " part ", tshow n,
                                              ": did=", did, ", upid=", _uploadID]
                                    lift $ storeUpload _gpFilePath mid up did (fromIntegral n) chunkSize
                                ) $ zip (NE.toList files) [1..]
      lift $ logInfo "Multipart upload created.  Use the 'upload' command to complete the upload."
      lift $ storeMetaBlob mid (typeOf (NE.head files)) files

    typeOf File{_gpCodec=GoProAVC}  = Video
    typeOf File{_gpCodec=GoProHEVC} = Video
    typeOf File{_gpCodec=GoPro360}  = Video
    typeOf File{_gpCodec=GoProJPG}  = Photo

    blobFor :: MediumType -> NonEmpty File -> IO (Maybe BS.ByteString)
    blobFor Photo (File{_gpFilePath} :| []) = BL.readFile _gpFilePath >>= (\case
          Left _  -> pure Nothing
          Right e -> pure $ Just (BL.toStrict e)) . minimalEXIF
    blobFor _ fs@(f :| _) = findGPMDStream (_gpFilePath f) >>=
        \case
          Nothing -> pure Nothing
          Just s  -> asum [ Just <$> extractGPMDStream (_gpFilePath <$> NE.toList fs) s, pure Nothing]

    storeMetaBlob mid typ fps = void . runMaybeT $ do
      blob <- liftIO $ blobFor typ fps
      lift $ insertMetaBlob mid mtype blob
      where
        mtype = case typ of
          Photo -> EXIF
          _     -> GPMF

runCreateMultipart :: ([Reader Env, LogFX, DatabaseEff, IOE] :>> es) => MediumType -> NonEmpty FilePath -> Eff es ()
runCreateMultipart typ fps = do
  runUpload fps $ do
    setMediumType typ
    chunkSize <- lift $ asksOpt optChunkSize
    setChunkSize chunkSize
    mid <- createMedium
    did <- createSource (length fps)
    c <- lift $ asksOpt optUploadConcurrency
    pooledMapConcurrentlyN_ c (\(fp,n) -> do
              fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
              up@Upload{..} <- createUpload did n (fromInteger fsize)
              lift $ logInfoL ["Creating part ", tshow fp, " as ", mid, " part ", tshow n,
                        ": did=", did, ", upid=", _uploadID]
              lift $ storeUpload fp mid up did (fromIntegral n) chunkSize
          ) $ zip (NE.toList fps) [1..]
    lift $ logInfo "Multipart upload created.  Use the 'upload' command to complete the upload."

runResumeUpload :: ([Reader Env, LogFX, DatabaseEff, IOE] :>> es) => Eff es ()
runResumeUpload = do
  ups <- listPartialUploads
  logInfoL ["Have ", tshow (length ups), " media items to upload in ",
            tshow (sum $ fmap length ups), " parts with a total of ",
            tshow (sumOf pu_size (fold ups)), " chunks (",
            tshow (sumOf pu_mb (fold ups)), " MB)"]
  mapM_ upAll ups
  where
    pu_size = length . _pu_parts
    pu_mb :: PartialUpload -> Int
    pu_mb p@PartialUpload{..} = (fromIntegral _pu_chunkSize `div` (1024*1024)) * pu_size p
    sumOf :: Foldable t => (a -> Int) -> t a -> Int
    sumOf f = getSum . foldMap (Sum . f)

    upAll [] = pure ()

    upAll xs@(PartialUpload{..}:_) = do
      logInfoL ["Uploading ", tshow _pu_medium_id, " in ", tshow (sumOf pu_size xs),
                " chunks (", tshow (sumOf pu_mb xs), " MB)"]
      mapM_ up xs
      logInfoL ["Finished uploading ", tshow _pu_medium_id]
      resumeUpload (_pu_filename :| []) _pu_medium_id $ markAvailable _pu_did

    up PartialUpload{..} = do
      let part = fromIntegral _pu_partnum
      fsize <- fromIntegral . fileSize <$> (liftIO . getFileStatus) _pu_filename
      resumeUpload (_pu_filename :| []) _pu_medium_id $ do
        setChunkSize _pu_chunkSize
        Upload{..} <- getUpload _pu_upid _pu_did part fsize
        let chunks = sortOn (Down . _uploadPart) $
                     filter (\UploadPart{..} -> _uploadPart `elem` _pu_parts) _uploadParts
        lift $ logInfoL ["Uploading ", tshow _pu_filename, " (", tshow fsize, " bytes) as ",
                  _pu_medium_id, ":", tshow part, ": did=",
                  _pu_did, ", upid=", _uploadID, ", parts=", tshow (length chunks)]
        c <- lift $ asksOpt optUploadConcurrency
        _ <- pooledMapConcurrentlyN c (uc _pu_filename _pu_medium_id _pu_partnum) chunks
        completeUpload _uploadID _pu_did part (fromIntegral fsize)
      completedUpload _pu_medium_id _pu_partnum

runReprocessCmd :: ([Reader Env, LogFX, DatabaseEff, IOE] :>> es) => NonEmpty MediumID -> Eff es ()
runReprocessCmd = mapM_ reprocess
