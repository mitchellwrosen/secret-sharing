{-# LANGUAGE RankNTypes #-}

module Control.Monad.CIO
  ( CIO(CIO)
  , lower
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

newtype CIO a
  = CIO { (>-) :: forall r. (a -> IO r) -> IO r }

lower :: CIO a -> IO a
lower = (>- pure)

instance Functor CIO where
  fmap f m = CIO (\k -> m >- \a -> k (f a))

instance Applicative CIO where
  pure = return
  (<*>) = ap

instance Monad CIO where
  return x = CIO (\k -> k x)
  m >>= f = CIO (\k -> m >- \a -> f a >- \b -> k b)

instance MonadIO CIO where
  liftIO m = CIO (m >>=)
