{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $  (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT emf) <*> (EitherT ema) = EitherT $ (fmap (<*>) emf) <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ mea >>= (\v -> case fmap f v of
                                              Left x -> pure $ Left x
                                              Right x -> runEitherT x)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea
  where
    swapEither :: Either e a -> Either a e
    swapEither (Left x) = Right x
    swapEither (Right x) = Left x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT mab) = mab >>= (\x -> case x of
                                            Left v -> amc v
                                            Right v -> bmc v)
