{-# LANGUAGE InstanceSigs #-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ (fmap (<*>) mmf) <*> mma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $ mma >>= (\v -> case v of
                                            Nothing -> pure Nothing
                                            (Just x) -> runMaybeT (f x))
