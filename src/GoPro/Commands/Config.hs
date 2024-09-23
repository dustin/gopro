{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Config (
  runListConfig, runGetConfig, runSetConfig
  ) where

import           Cleff
import           Cleff.Reader
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text.IO    as TIO

import           GoPro.Commands
import           GoPro.DB

runListConfig :: ([Reader Env, DatabaseEff, IOE] :>> es) => Eff es ()
runListConfig = mapM_ (\(k,v) -> mapM_ (liftIO . TIO.putStr) [optionStr k, " = ", v, "\n"]) . Map.assocs =<< asks gpConfig

runGetConfig :: ([Reader Env, DatabaseEff, IOE] :>> es) => ConfigOption -> Eff es ()
runGetConfig k = asks (configItem k) >>= liftIO . TIO.putStrLn

runSetConfig :: ([Reader Env, DatabaseEff, IOE] :>> es) => ConfigOption -> Text -> Eff es ()
runSetConfig k v = ask >>= \Env{..} -> updateConfig $ Map.insert k v gpConfig
