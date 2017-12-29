{-# LANGUAGE InstanceSigs #-}

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

-- instance Applicative m => Applicative (ReaderT r m) where
  
