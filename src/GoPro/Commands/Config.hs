{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Config (
  runListConfig, runGetConfig, runSetConfig
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ask, asks)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO

import           GoPro.Commands
import           GoPro.DB

runListConfig :: GoPro ()
runListConfig = mapM_ (\(k,v) -> mapM_ (liftIO . TIO.putStr) [optionStr k, " = ", v, "\n"]) . Map.assocs =<< asks gpConfig

runGetConfig :: ConfigOption -> GoPro ()
runGetConfig k = asks (configItem k) >>= liftIO . TIO.putStrLn

runSetConfig :: MonadIO m => ConfigOption -> Text -> GoProT m ()
runSetConfig k v = ask >>= \Env{..} -> updateConfig database $ Map.insert k v gpConfig
