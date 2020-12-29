{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Config (
  runListConfig, runGetConfig, runSetConfig
  ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO

import           GoPro.Commands
import           GoPro.DB

runListConfig :: GoPro ()
runListConfig = mapM_ (\(k,v) -> mapM_ (liftIO . TIO.putStr) [k, " = ", v, "\n"]) . Map.assocs =<< asks gpConfig

runGetConfig :: Text -> GoPro ()
runGetConfig k = asks (configItem k) >>= liftIO . TIO.putStrLn

runSetConfig :: Text -> Text -> GoPro ()
runSetConfig k v = do
  cfg <- asks gpConfig
  unless (Map.member k cfg) $ fail ("invalid key " <> show k)
  updateConfig (Map.insert k v cfg)
