{-# LANGUAGE InstanceSigs #-}
newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative f => Applicative (IdentityT f) where
  pure x = IdentityT $ pure x
  (IdentityT f) <*> (IdentityT fa) = IdentityT $ f <*> fa

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
