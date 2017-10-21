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

-- then the actual zippy thingy

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

-- a little helper function
zipCons :: a -> (ZipList' a) -> (ZipList' a)
zipCons x (ZipList' zl) = ZipList' (Cons x zl)

instance (Monoid a) => Monoid (ZipList' a) where
  mempty = ZipList' Nil
  mappend (ZipList' Nil) l = l
  mappend l (ZipList' Nil) = l
  mappend (ZipList' (Cons x xs)) (ZipList' (Cons y ys)) =
    zipCons (x `mappend` y) (mappend (ZipList' xs) (ZipList' ys))

instance Functor ZipList' where
  fmap _ (ZipList' Nil) = ZipList' Nil
  fmap f (ZipList' (Cons x xs)) = zipCons (f x) $ fmap f (ZipList' xs)

instance Applicative ZipList' where
  pure f = ZipList' (Cons f Nil)
  (ZipList' Nil) <*> xs = ZipList' Nil
  fs <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = zipCons (f x) ((ZipList' fs) <*> (ZipList' xs))
  
lst = Cons 1 (Cons 2 (Cons 3 Nil))
zLst = ZipList' lst
fzLst = ZipList' (Cons (+1) (Cons (*2) Nil))


