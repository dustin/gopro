{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Upload where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Text              as T
import           System.Posix.Files     (fileSize, getFileStatus)

import           GoPro.Commands
import           GoPro.Plus.Upload

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
    setMediumType (read typ)
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
