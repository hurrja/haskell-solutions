import Control.Applicative
import Data.Semigroup
import Data.Monoid (mappend)

data List a = Nil | Cons a (List a) deriving (Eq, Show)
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

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)

instance (Monoid a) => Monoid (ZipList a) where
  mempty = undefined
  mappend = undefined
  
