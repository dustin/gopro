{-# LANGUAGE FlexibleContexts #-}

module GoPro.Commands.Config (
  runConfig
  ) where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (asks)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO

import           GoPro.Commands
import           GoPro.DB

runConfig :: [String] -> GoPro ()
runConfig = exec
  where
    exec []     = listConfig
    exec [k]    = showConfig k
    exec [k, v] = setConfig k v
    exec _      = fail "expected exactly 0, 1, or 2 arguments"

    listConfig = mapM_ (\(k,v) -> mapM_ (liftIO . TIO.putStr) [k, " = ", v, "\n"]) . Map.assocs =<< asks gpConfig

    showConfig k = asks (configItem (T.pack k)) >>= liftIO . TIO.putStrLn

    setConfig k v = do
      cfg <- asks gpConfig
      let kt = T.pack k
          kv = T.pack v
      unless (Map.member kt cfg) $ fail ("invalid key " <> k)
      updateConfig (Map.insert kt kv cfg)
