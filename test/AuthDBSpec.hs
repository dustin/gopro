{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module AuthDBSpec where

import           Database.SQLite.Simple

import           Test.QuickCheck.Instances.Text ()
import           Test.Tasty
import           Test.Tasty.QuickCheck          as QC

import           GoPro.Plus.Auth

import           GoPro.AuthDB

deriving instance Eq AuthInfo

instance Arbitrary AuthInfo where
  arbitrary = AuthInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_authStorage :: NonEmptyList AuthInfo -> Property
prop_authStorage (NonEmpty ais) = ioProperty . withConnection ":memory:" $ \db -> do
  mapM_ (updateAuth db) ais
  loaded <- loadAuth db
  pure (loaded === (last ais))
