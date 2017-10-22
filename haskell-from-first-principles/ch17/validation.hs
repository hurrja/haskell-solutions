data Validation a b = Err a | OK b deriving (Eq, Show)

instance Functor (Validation a) where
  fmap _ (Err x) = Err x
  fmap f (OK x) = OK $ f x

instance (Monoid a) => Applicative (Validation a) where
  pure = OK
  (OK f) <*> (OK x) = OK $ f x
  (OK f) <*> (Err x) = Err x
  (Err f) <*> (OK x) = Err f
  (Err f) <*> (Err x) = Err $ mappend f x
