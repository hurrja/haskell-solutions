newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity x) = Identity $ f x
instance Foldable Identity where
  foldr f acc (Identity x) = f x acc
instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b = Constant { getConstant :: a}
instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x
instance Foldable (Constant a) where
  foldr _ acc _ = acc
instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

data Optional a = Njet | Da a
instance Functor Optional where
  fmap f (Da x) = Da $ f x
  fmap _ Njet = Njet
instance Foldable Optional where
  foldr f acc (Da x) = f x acc
  foldr _ acc Njet = acc
instance Traversable Optional where
  traverse f (Da x) = Da <$> f x
  traverse _ Njet = pure Njet

data List a = Nil | Cons a (List a) deriving Show
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs
instance Foldable List where
  foldr _ acc Nil = acc
  foldr f acc (Cons x xs) = f x $ foldr f acc xs
instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = (Cons <$> f x) <*> (traverse f xs)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z
instance Foldable (Three a b) where
  foldr f acc (Three _ _ x) = f x acc
instance Traversable (Three a b) where
  traverse f (Three x y z) = (Three x y) <$> f z

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
instance Foldable (Three' a) where
  foldr f acc (Three' _ x y) = f x $ f y acc
instance Traversable (Three' a) where
  traverse f (Three' x y z) = (Three' x) <$> f y <*> f z

data S n a = S (n a) a
instance Functor n => Functor (S n) where
  fmap f (S x y) = S (f <$> x) $ f y
instance Foldable n => Foldable (S n) where
  foldr f acc (S x y) = foldr f (f y acc) x
instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y
