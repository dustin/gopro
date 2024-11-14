{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GoPro.AuthCache where

import           Cleff

import           Data.Cache      (Cache (..), fetchWithCache, insert, newCache)
import           GoPro.Plus.Auth
import           System.Clock    (TimeSpec (..))
import           UnliftIO        (bracket_)
import           UnliftIO.MVar   (MVar, newEmptyMVar, putMVar, takeMVar)

import           GoPro.DB
import           GoPro.Logging

data AuthCacheData = AuthCacheData
    { authCache :: Cache () AuthInfo
    , authMutex :: MVar ()
    }

data AuthCache :: Effect where
    GetAuthInfo :: AuthCache m AuthInfo
    SetAuthInfo :: AuthInfo -> AuthCache m ()

makeEffect ''AuthCache

newAuthCacheData :: IOE :> es => Eff es AuthCacheData
newAuthCacheData = liftIO $ AuthCacheData <$> newCache (Just (TimeSpec 60 0)) <*> newEmptyMVar

runAuthCache :: [DB, LogFX, IOE] :>> es => AuthCacheData -> Eff (AuthCache : es) a -> Eff es a
runAuthCache acd@AuthCacheData{..} = interpret $ \case
  GetAuthInfo -> authMutexed acd $ fetchWithCache authCache () (const fetchOrRefresh)
  SetAuthInfo ai -> liftIO $ insert authCache () ai

authMutexed :: (IOE :> es) => AuthCacheData -> Eff es a -> Eff es a
authMutexed AuthCacheData{..} = bracket_ (putMVar authMutex ()) (takeMVar authMutex)

fetchOrRefresh :: ([DB, LogFX, IOE] :>> es) => Eff es AuthInfo
fetchOrRefresh = do
  logDbg "Reading auth token from DB"
  AuthResult ai expired <- loadAuth
  if expired then do
    logDbg "Refreshing auth info"
    res <- refreshAuth ai
    updateAuth res
    pure res
  else
    pure ai

instance (AuthCache :> es) => HasGoProAuth (Eff es) where
  goproAuth = getAuthInfo
