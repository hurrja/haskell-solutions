data Nope a = Nope deriving Show

instance Functor Nope where
  fmap _ Nope = Nope
  
instance Applicative Nope where
  pure _ = Nope
  _ <*> _ = Nope

instance Monad Nope where
  return = pure
  Nope >>= _ = Nope

--
data DifEither b a = Lefty a | Righty b deriving Show

instance Functor (DifEither b) where
  fmap f (Lefty x) = Lefty $ f x
  fmap _ (Righty x) = Righty x

instance Applicative (DifEither b) where
  pure = Lefty
  (Lefty f) <*> (Lefty x) = Lefty $ f x
  (Lefty _) <*> (Righty x) = Righty x
  (Righty x) <*> _ = Righty x

instance Monad (DifEither b) where
  return = pure
  (Lefty x) >>= f = f x
  (Righty x) >>= _ = Righty x
