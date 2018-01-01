{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

fstMap :: (a -> b) -> (a, c) -> (b, c)
fstMap f (x, y) = (f x, y)

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s -> fmap (fstMap f) (smas s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smfs) <*> (StateT smas) = StateT $ \s -> smfs s >>= \(f, s2) -> fmap (fstMap f) $ smas s2
  
instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT smas) >>= f = StateT $ \s -> smas s >>= \(a, s2) -> runStateT (f a) s2
  
instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> liftM (\a -> (a, s)) ma
