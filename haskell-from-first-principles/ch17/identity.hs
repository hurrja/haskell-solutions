newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure f = Identity f
  (Identity f) <*> (Identity x) = Identity $ f x
  
