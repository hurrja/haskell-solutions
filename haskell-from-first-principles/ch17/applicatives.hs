newtype Identity a = Identity a deriving Show

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

--

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

--

data Two a b = Two a b deriving Show

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two _ f) <*> (Two x y) = Two x $ f y

--

data Three a b c = Three a b c deriving Show

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three _ _ f) <*> (Three x y z) = Three x y $ f z

--

data Three' a b = Three' a b b deriving Show

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' _ f g) <*> (Three' x y z) = Three' x (f y) (g z)
  
-- Four and Four' would be straightforward extensions
