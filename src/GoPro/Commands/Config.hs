{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Config (
  runListConfig, runGetConfig, runSetConfig
  ) where

import           Cleff
import           Data.Foldable (traverse_)
import           Data.Text     (Text)
import qualified Data.Text.IO  as TIO

import           GoPro.Config
import           GoPro.DB

runListConfig :: ([IOE, ConfigFX] :>> es) => Eff es ()
runListConfig = traverse_ (\k -> configItem k >>= \v -> traverse_ (liftIO . TIO.putStr) [optionStr k, " = ", v, "\n"]) [minBound..]

runGetConfig :: ([IOE, ConfigFX] :>> es) => ConfigOption -> Eff es ()
runGetConfig k = configItem k >>= liftIO . TIO.putStrLn

runSetConfig :: ([ConfigFX, DatabaseEff, IOE] :>> es) => ConfigOption -> Text -> Eff es ()
runSetConfig k v = configSet k v
