data Constant a b = Constant a
instance Foldable (Constant a) where
  foldr _ acc (Constant _) = acc

data Two a b = Two a b
instance Foldable (Two a) where
  foldr f acc (Two _ x) = f x acc

data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldr f acc (Three _ _ x) = f x acc

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldr f acc (Three' _ x y) = f x $ f y acc

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldr f acc (Four' _ x y z) = f x $ f y $ f z acc
