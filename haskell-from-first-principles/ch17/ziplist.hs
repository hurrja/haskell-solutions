import Control.Applicative
import Data.Monoid ((<>))

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Monoid (List a) where
  mempty = Nil
  mappend Nil l = l
  mappend l Nil = l
  mappend (Cons x xs) l = Cons x (xs <> l)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) =  Cons (f x) ((f <$> xs) `mappend` (fs <*> (Cons x xs)))

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)

instance (Monoid a) => Monoid (ZipList a) where
  mempty = undefined
  mappend = undefined
  
