module GoPro.S3 where

import           Codec.Compression.GZip  (compress)
import           Control.Lens
import           Control.Monad           (void)
import           Control.Monad.Trans.AWS (Credentials (..), Region (..),
                                          envRegion, newEnv, paginate, runAWST,
                                          runResourceT, send, AWST')
import qualified Data.ByteString.Lazy    as BL
import           Data.Conduit            (runConduit, (.|))
import Control.Monad.Catch (MonadCatch(..))
import UnliftIO (MonadUnliftIO(..))
import qualified Data.Conduit.List       as CL
import           Data.String             (fromString)
import           Data.Text               (Text, isSuffixOf, pack, unpack)
import           Network.AWS.Data.Body   (RqBody (..), ToHashedBody (..))
import           Network.AWS.S3
import           System.FilePath.Posix   (takeBaseName, takeDirectory)
import qualified Network.AWS.Env as AWSE
import Control.Monad.Trans.Resource (ResourceT)

import           GoPro.Commands
import           GoPro.Plus.Media

type Derivative = (MediumID, Text)

inAWS :: (MonadCatch m, MonadUnliftIO m) => Region -> AWST' AWSE.Env (ResourceT m) a -> m a
inAWS r a = (newEnv Discover <&> set envRegion r) >>= \awsenv -> (runResourceT . runAWST awsenv) a

allDerivatives :: BucketName -> GoPro [Derivative]
allDerivatives bucketName = inAWS Oregon $
    runConduit $ paginate (listObjectsV2 bucketName & lovPrefix ?~ "derivatives/")
    .| CL.concatMap (view lovrsContents)
    .| CL.map (view (oKey . _ObjectKey))
    .| CL.filter (not . ("/" `isSuffixOf`))
    .| CL.map toDir
    .| CL.consume

  where toDir t = let s = unpack t in
                    (pack . takeBaseName . takeDirectory $ s, pack $ takeBaseName s)

storeMetaBlob :: BucketName -> MediumID -> BL.ByteString -> GoPro ()
storeMetaBlob bucketName mid blob = do
  let key = fromString $ "metablob/" <> unpack mid <> ".gz"
  logInfo $ "Storing metadata blob at " <> tshow key
  inAWS Oregon $ void . send $ putObject bucketName key (Hashed . toHashed . compress $ blob)
