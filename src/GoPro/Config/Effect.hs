{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.Config.Effect where

import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.Fail
import           Effectful.State.Dynamic
import           Effectful.TH

import           Data.Map            (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)

import           GoPro.Config.Option
import           GoPro.DB

data ConfigFX :: Effect where
    ConfigGet  :: ConfigOption -> Text -> ConfigFX m Text
    ConfigSet  :: ConfigOption -> Text -> ConfigFX m ()

makeEffect ''ConfigFX

runConfig :: DB :> es => Map ConfigOption Text -> Eff (ConfigFX : es) a -> Eff es a
runConfig cfg = reinterpret_ (evalStateLocal cfg) \case
  ConfigGet k def -> Map.findWithDefault def k <$> get
  ConfigSet k v -> do
    modify $ Map.insert k v
    updateConfig =<< get

configItem :: ConfigFX :> es => ConfigOption -> Eff es Text
configItem k = configGet k ""

configItemDef :: ConfigFX :> es => ConfigOption -> Text -> Eff es Text
configItemDef = configGet
