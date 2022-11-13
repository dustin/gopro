{-# LANGUAGE ScopedTypeVariables #-}

module GoPro.ConfigFile (loadConfigFile) where

import           Control.Applicative    ((<|>))
import           Control.Exception      (catch)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (fromMaybe)
import qualified Toml
import           Toml                   (TomlCodec, (.=))

import           GoPro.Commands

data Config = Config
  { dbPath              :: Maybe String
  , staticPath          :: Maybe FilePath
  , uploadConcurrency   :: Maybe Int
  , downloadConcurrency :: Maybe Int
  , chunkSize           :: Maybe Integer
  , referenceDir        :: Maybe FilePath
  } deriving Show

emptyConfig :: Config
emptyConfig = Config Nothing Nothing Nothing Nothing Nothing Nothing

codec :: TomlCodec Config
codec = Config
    <$> Toml.dioptional (Toml.string "database")       .= dbPath
    <*> Toml.dioptional (Toml.string "staticPath") .= staticPath
    <*> Toml.dioptional (Toml.int "upload.concurrency") .= uploadConcurrency
    <*> Toml.dioptional (Toml.int "download.concurrency") .= downloadConcurrency
    <*> Toml.dioptional (Toml.integer "upload.chunkSize") .= chunkSize
    <*> Toml.dioptional (Toml.string "referenceDir") .= referenceDir

readConfig :: MonadIO m => FilePath -> m Config
readConfig fn = liftIO $ catch (Toml.decodeFile codec fn) (\(_ :: IOError) -> pure emptyConfig)

mergeConfig :: Options -> Config -> Options
mergeConfig opt@Options{..} Config{..} = opt{
  optDBPath = fromMaybe optDBPath dbPath,
  optStaticPath = fromMaybe optStaticPath staticPath,
  optUploadConcurrency = fromMaybe optUploadConcurrency uploadConcurrency,
  optDownloadConcurrency = fromMaybe optDownloadConcurrency downloadConcurrency,
  optChunkSize = fromMaybe optChunkSize chunkSize,
  optReferenceDir = referenceDir <|> optReferenceDir
  }

-- | Load a TOML config file with the
loadConfigFile :: MonadIO m => Options -> FilePath -> m Options
loadConfigFile o = fmap (mergeConfig o) . readConfig
