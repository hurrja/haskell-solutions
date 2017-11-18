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
