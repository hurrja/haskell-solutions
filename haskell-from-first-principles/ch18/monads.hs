import Data.Semigroup

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

--

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

--

data List a = Nil | Cons a (List a) deriving Show

instance Semigroup (List a) where
  Nil <> l = l
  l <> Nil = l
  (Cons x xs) <> l = Cons x (xs <> l)
instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs =  (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = (f x) <> (xs >>= f)
