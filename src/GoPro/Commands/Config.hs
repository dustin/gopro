{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Config (
  runListConfig, runGetConfig, runSetConfig
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO

import           GoPro.Commands
import           GoPro.DB

runListConfig :: GoPro ()
runListConfig = mapM_ (\(k,v) -> mapM_ (liftIO . TIO.putStr) [optionStr k, " = ", v, "\n"]) . Map.assocs =<< asks gpConfig

runGetConfig :: ConfigOption -> GoPro ()
runGetConfig k = asks (configItem k) >>= liftIO . TIO.putStrLn

runSetConfig :: ConfigOption -> Text -> GoPro ()
runSetConfig k v = asks gpConfig >>= updateConfig . Map.insert k v
