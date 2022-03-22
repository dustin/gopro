{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module GoPro.Commands.Upload (
  runCreateUploads, runCreateMultipart, runResumeUpload, runReprocessCmd
  ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import           Data.Foldable          (fold)
import           Data.List              (sortOn)
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NE
import           Data.Monoid            (Sum (..))
import           Data.Ord               (Down (..))
import qualified Data.Text              as T
import           Data.These             (These (..), these)
import           System.Posix.Files     (fileSize, getFileStatus)

import           GoPro.Commands
import           GoPro.DB
import           GoPro.File
import           GoPro.Plus.Media       (MediumID, MediumType (..), reprocess)
import           GoPro.Plus.Upload

uc :: FilePath -> MediumID -> Integer -> UploadPart -> Uploader GoPro ()
uc fp mid partnum up@UploadPart{..} = do
  logDbgL ["Uploading part ", tshow _uploadPart, " of ", T.pack fp]
  uploadChunk fp up
  completedUploadPart mid _uploadPart partnum
  logDbgL ["Finished part ", tshow _uploadPart, " of ", T.pack fp]

runCreateUploads :: NonEmpty FilePath -> GoPro ()
runCreateUploads filePaths = do
  -- Exclude any commandline params for files that are already being
  -- uploaded.  This prevents duplicate uploads if you just hit
  -- up-enter, but it also prevents one from uploading a file if it's
  -- already included in a multipart upload.
  queued <- listQueuedFiles
  let candidates = filter (`notElem` queued) (NE.toList filePaths)
      (bad, good) = these (,[]) ([],) (,) $ maybe (This []) parseAndGroup (NE.nonEmpty candidates)

  when (not . null $ bad) $ logInfoL ["Ignoring some unknown files: ", tshow bad]

  db <- goproDB
  c <- asks (optUploadConcurrency . gpOptions)
  mapConcurrentlyLimited_ c (upload db) good

  where
    upload db files = asks (optChunkSize . gpOptions) >>= \chunkSize -> runUpload (_gpFilePath <$> files) $ do
      setMediumType (typeOf (NE.head files))
      setChunkSize chunkSize
      mid <- createMedium
      did <- createSource (length files)
      c <- asks (optUploadConcurrency . gpOptions)
      mapConcurrentlyLimited_ c (\(File{_gpFilePath},n) -> do
                                    fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) _gpFilePath
                                    up@Upload{..} <- createUpload did n (fromInteger fsize)
                                    logInfoL ["Creating part ", tshow _gpFilePath, " as ", mid, " part ", tshow n,
                                              ": did=", did, ", upid=", _uploadID]
                                    liftIO . withDB db $ storeUpload _gpFilePath mid up did (fromIntegral n) chunkSize
                                ) $ zip (NE.toList files) [1..]
      logInfo "Multipart upload created.  Use the 'upload' command to complete the upload."

    typeOf File{_gpCodec=GoProAVC}  = Video
    typeOf File{_gpCodec=GoProHEVC} = Video
    typeOf File{_gpCodec=GoProJPG}  = Photo

runCreateMultipart :: MediumType -> NonEmpty FilePath -> GoPro ()
runCreateMultipart typ fps = do
  db <- goproDB
  runUpload fps $ do
    setMediumType typ
    chunkSize <- asks (optChunkSize . gpOptions)
    setChunkSize chunkSize
    mid <- createMedium
    did <- createSource (length fps)
    c <- asks (optUploadConcurrency . gpOptions)
    mapConcurrentlyLimited_ c (\(fp,n) -> do
              fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
              up@Upload{..} <- createUpload did n (fromInteger fsize)
              logInfoL ["Creating part ", tshow fp, " as ", mid, " part ", tshow n,
                        ": did=", did, ", upid=", _uploadID]
              liftIO . withDB db $ storeUpload fp mid up did (fromIntegral n) chunkSize
          ) $ zip (NE.toList fps) [1..]
    logInfo "Multipart upload created.  Use the 'upload' command to complete the upload."

runResumeUpload :: GoPro ()
runResumeUpload = do
  ups <- listPartialUploads
  logInfoL ["Have ", tshow (length ups), " media items to upload in ",
            tshow (sum $ fmap length ups), " parts with a total of ",
            tshow (sumOf pu_size ups), " chunks (",
            tshow (sumOf pu_mb ups), " MB)"]
  mapM_ upAll ups
  where
    pu_size = length . _pu_parts
    pu_mb :: PartialUpload -> Int
    pu_mb p@PartialUpload{..} = (fromIntegral _pu_chunkSize `div` (1024*1024)) * pu_size p
    sumOf :: (a -> Int) -> [[a]] -> Int
    sumOf f = getSum . fold . foldMap (fmap (Sum . f))

    upAll [] = pure ()

    upAll xs@(PartialUpload{..}:_) = do
      logInfoL ["Uploading ", tshow _pu_medium_id, " in ", tshow (sumOf pu_size [xs]),
                " chunks (", tshow (sum . fmap pu_mb $ xs), " MB)"]
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
        logInfoL ["Uploading ", tshow _pu_filename, " (", tshow fsize, " bytes) as ",
                  _pu_medium_id, ":", tshow part, ": did=",
                  _pu_did, ", upid=", _uploadID, ", parts=", tshow (length chunks)]
        c <- asks (optUploadConcurrency . gpOptions)
        _ <- mapConcurrentlyLimited c (uc _pu_filename _pu_medium_id _pu_partnum) chunks
        completeUpload _uploadID _pu_did part (fromIntegral fsize)
      completedUpload _pu_medium_id _pu_partnum

runReprocessCmd :: NonEmpty MediumID -> GoPro ()
runReprocessCmd = mapM_ reprocess
