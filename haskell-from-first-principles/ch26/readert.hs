{-# LANGUAGE InstanceSigs #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (fmap (<*>) rmab) <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> rma r >>= \a -> (runReaderT (f a)) r
  
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
--  liftIO i = ReaderT $ \_ -> liftIO i
--  liftIO i = ReaderT $ const (liftIO i)
--  liftIO i = ReaderT . const $ liftIO i
--  liftIO i = lift $ liftIO i
  liftIO = lift . liftIO

foo :: ReaderT String Maybe Int
foo = lift $ Just 1

bar :: Maybe Int
bar = (runReaderT $ foo >>= pure . (1+)) "abc"
