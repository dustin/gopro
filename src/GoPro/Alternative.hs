{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module GoPro.Alternative where

import           Control.Applicative (Alternative (..))
import           Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import           Data.Foldable       (asum)
import           UnliftIO.Exception  (SomeException)


-- TryAlternative is a newtype wrapper around a monad that provides an Alternative instance via exception handling.
newtype TryAlternative m a = TryAlternative { runTryAlternative :: m a }
  deriving (Functor, Applicative, Monad)

instance (MonadCatch m, MonadThrow (TryAlternative m)) => MonadCatch (TryAlternative m) where
  catch (TryAlternative x) f = TryAlternative $ catch x (runTryAlternative . f)

-- Provide an Alternative instance where failure is due to exceptions
instance (MonadCatch m, MonadFail m) => Alternative (TryAlternative m) where
  empty = TryAlternative $ catch (fail "empty") (\(_ :: SomeException) -> fail "caught exception")
  TryAlternative x <|> TryAlternative y = TryAlternative $ catch x (\(_ :: SomeException) -> y)

tsum :: (MonadCatch f, MonadFail f, Functor t, Foldable t) => t (f a) -> f a
tsum = runTryAlternative . asum . fmap TryAlternative
