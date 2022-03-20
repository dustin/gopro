{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module GoPro.S3 where

import           Amazonka                     (Credentials (Discover), Env, Region (..), RequestBody (Hashed), newEnv,
                                               paginate, runResourceT, send, sinkBody, toHashed)
import           Amazonka.S3                  (BucketName (..), StorageClass (..), _ObjectKey, newGetObject,
                                               newListObjectsV2, newPutObject)
import           Codec.Compression.GZip       (compress)
import           Control.Lens
import           Control.Monad                (void)
import           Control.Monad.Reader         (asks)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 (runConduit, (.|))
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Zlib            (ungzip)
import           Data.Generics.Labels         ()
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (fromString)
import           Data.Text                    (Text, isSuffixOf, pack, unpack)
import           System.FilePath.Posix        (takeBaseName, takeDirectory)
import           UnliftIO                     (MonadUnliftIO (..))

import           GoPro.Commands
import           GoPro.DB                     (ConfigOption (..))
import           GoPro.Plus.Media

type Derivative = (MediumID, Text)

inAWS :: MonadUnliftIO m => (Amazonka.Env -> ResourceT m b) -> m b
inAWS a = (newEnv Discover <&> set #_envRegion Oregon) >>= runResourceT . a

s3Bucket :: GoPro BucketName
s3Bucket = do
  b <- asks (BucketName . configItem CfgBucket)
  if b == "" then fail "s3 bucket is not configured" else pure b

allDerivatives :: GoPro [Derivative]
allDerivatives = s3Bucket >>= \b -> inAWS $ \env ->
  runConduit $ paginate env (newListObjectsV2 b & #prefix ?~ "derivatives/")
    .| CL.concatMap (view (#contents . _Just))
    .| CL.map (view (#key . _ObjectKey))
    .| CL.filter (not . ("/" `isSuffixOf`))
    .| CL.map toDir
    .| CL.consume

  where toDir t = let s = unpack t in
                    (pack . takeBaseName . takeDirectory $ s, pack $ takeBaseName s)

getMetaBlob :: MediumID -> GoPro BL.ByteString
getMetaBlob mid = do
  b <- s3Bucket
  let key = fromString $ "metablob/" <> unpack mid <> ".gz"
  logDbgL ["Requesting metablob from S3: ", tshow key]
  inAWS $ \env -> do
    rs <- send env (newGetObject b key)
    (rs ^. #body) `sinkBody` (ungzip .| CB.sinkLbs)

storeMetaBlob :: MediumID -> Maybe BL.ByteString -> GoPro ()
storeMetaBlob mid blob = do
  b <- s3Bucket
  let key = fromString $ "metablob/" <> unpack mid <> ".gz"
  logInfoL ["Storing metadata blob at ", tshow key]
  inAWS $ \env -> void . send env $ newPutObject b key (Hashed . toHashed . compress . fromMaybe "" $ blob) &
    #storageClass ?~ StorageClass' "STANDARD_IA"

listMetaBlobs :: GoPro [MediumID]
listMetaBlobs = s3Bucket >>= \b -> inAWS $ \env ->
  runConduit $ paginate env (newListObjectsV2 b & #prefix ?~ "metablob/")
    .| CL.concatMap (view (#contents . _Just))
    .| CL.map (view (#key . _ObjectKey))
    .| CL.filter (".gz" `isSuffixOf`)
    .| CL.map (pack . takeBaseName . unpack)
    .| CL.consume
