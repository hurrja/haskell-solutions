data Either' a b = L a | R b deriving (Eq, Show)

instance Functor (Either' a) where
  fmap _ (L x) = L x
  fmap f (R x) = R $ f x

instance Applicative (Either' a) where
  pure = R
  (L f) <*> _ = L f
  (R _) <*> (L x) = L x
  (R f) <*> (R x) = R $ f x
