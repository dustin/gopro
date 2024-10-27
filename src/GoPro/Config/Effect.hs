{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.Config.Effect where

import           Cleff
import           Cleff.State

import           Data.Map            (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)

import           GoPro.Config.Option
import           GoPro.DB

data ConfigFX :: Effect where
    ConfigGet  :: ConfigOption -> Text -> ConfigFX m Text
    ConfigSet  :: ConfigOption -> Text -> ConfigFX m ()

makeEffect ''ConfigFX

runConfig :: DatabaseEff :> es => Map ConfigOption Text -> Eff (ConfigFX : es) a -> Eff es a
runConfig cfg = fmap fst . runState cfg . reinterpret \case
  ConfigGet k def -> Map.findWithDefault def k <$> get
  ConfigSet k v -> do
    modify $ Map.insert k v
    updateConfig =<< get

configItem :: ConfigFX :> es => ConfigOption -> Eff es Text
configItem k = configGet k ""

configItemDef :: ConfigFX :> es => ConfigOption -> Text -> Eff es Text
configItemDef = configGet
