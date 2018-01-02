{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ (fmap (<*>) mmf) <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $ mma >>= (\v -> case fmap f v of
                                                 Nothing -> pure Nothing
                                                 Just x -> runMaybeT x)

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadIO m => MonadIO (MaybeT m) where
  -- evolution from what makes sense to the shortest form
--  liftIO i = MaybeT $ fmap Just (liftIO i)
--  liftIO i = MaybeT $ liftM Just (liftIO i)
--  liftIO i = (MaybeT . liftM Just) (liftIO i)
--  liftIO i = lift (liftIO i)
  liftIO = lift . liftIO
