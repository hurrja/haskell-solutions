data Either' a b = First a | Second b deriving (Eq, Show)

instance Functor (Either' a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Either' a) where
  pure = Second
  (Second f) <*> (Second x) = Second $ f x
  (Second _) <*> (First x) = First x
  (First x) <*> _ = First x
  
instance Monad (Either' a) where
  return = pure
  (Second x) >>= f = f x
  (First x) >>= _ = First x
