{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoPro.S3 where

import           Amazonka                     (Env, Region (..), RequestBody (Hashed), newEnv, paginate, runResourceT,
                                               send, sinkBody, toHashed)
import           Amazonka.Auth                (discover)
import           Amazonka.S3                  (BucketName (..), StorageClass (..), _ObjectKey, newGetObject,
                                               newListObjectsV2, newPutObject)
import           Cleff                        hiding (send)
import           Cleff.Fail
import           Codec.Compression.GZip       (compress)
import           Control.Lens
import           Control.Monad                (void)
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
import qualified Data.Text                    as T
import           System.FilePath.Posix        (takeBaseName, takeDirectory)

import           Control.Monad.Catch          (MonadCatch)
import           GoPro.Logging
import           GoPro.Plus.Media

type Derivative = (MediumID, Text)

inAWS :: (MonadCatch m, MonadUnliftIO m) => (Amazonka.Env -> ResourceT m b) -> m b
inAWS a = (newEnv discover <&> set #region Oregon) >>= runResourceT . a


data S3 :: Effect where
  GetMetaBlob :: MediumID -> S3 m BL.ByteString
  StoreMetaBlob :: MediumID -> Maybe BL.ByteString -> S3 m ()
  ListMetaBlobs :: S3 m [MediumID]
  AllDerivatives :: S3 m [Derivative]
  S3Bucket :: S3 m BucketName

makeEffect ''S3

runS3 :: [IOE, LogFX, Fail] :>> es => BucketName -> Eff (S3 : es) a -> Eff es a
runS3 b = interpret \case
    GetMetaBlob mid -> getMetaBlob' b mid
    StoreMetaBlob mid blob -> storeMetaBlob' b mid blob
    ListMetaBlobs -> listMetaBlobs' b
    AllDerivatives -> allDerivatives' b
    S3Bucket -> pure b

unconfiguredS3 :: Fail :> es => Eff (S3 : es) a -> Eff es a
unconfiguredS3 = interpret \case
  GetMetaBlob _ -> u
  StoreMetaBlob _ _ -> u
  ListMetaBlobs -> u
  AllDerivatives -> u
  S3Bucket -> u

  where
    u :: Fail :> es => Eff es a
    u = fail "s3 bucket is not configured"

allDerivatives' :: ([Fail, IOE] :>> es) => BucketName -> Eff es [Derivative]
allDerivatives' b = inAWS $ \env ->
  runConduit $ paginate env (newListObjectsV2 b & #prefix ?~ "derivatives/")
    .| CL.concatMap (view (#contents . _Just))
    .| CL.map (view (#key . _ObjectKey))
    .| CL.filter (not . ("/" `isSuffixOf`))
    .| CL.map toDir
    .| CL.consume

  where toDir t = let s = unpack t in
                    (pack . takeBaseName . takeDirectory $ s, pack $ takeBaseName s)

getMetaBlob' :: ([Fail, LogFX, IOE] :>> es) => BucketName -> MediumID -> Eff es BL.ByteString
getMetaBlob' b mid = do
  let key = fromString $ "metablob/" <> unpack mid <> ".gz"
  logDbgL ["Requesting metablob from S3: ", (T.pack . show) key]
  inAWS $ \env -> do
    rs <- send env (newGetObject b key)
    (rs ^. #body) `sinkBody` (ungzip .| CB.sinkLbs)

storeMetaBlob' :: ([Fail, LogFX, IOE] :>> es) => BucketName -> MediumID -> Maybe BL.ByteString -> Eff es ()
storeMetaBlob' b mid blob = do
  let key = fromString $ "metablob/" <> unpack mid <> ".gz"
  logInfoL ["Storing metadata blob at ", (T.pack . show) key]
  inAWS $ \env -> void . send env $ newPutObject b key (Hashed . toHashed . compress . fromMaybe "" $ blob) &
    #storageClass ?~ StorageClass_STANDARD_IA

listMetaBlobs' :: ([Fail, LogFX, IOE] :>> es) => BucketName -> Eff es [MediumID]
listMetaBlobs' b = inAWS $ \env ->
  runConduit $ paginate env (newListObjectsV2 b & #prefix ?~ "metablob/")
    .| CL.concatMap (view (#contents . _Just))
    .| CL.map (view (#key . _ObjectKey))
    .| CL.filter (".gz" `isSuffixOf`)
    .| CL.map (pack . takeBaseName . unpack)
    .| CL.consume
