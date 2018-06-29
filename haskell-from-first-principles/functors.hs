data MyContainer a = MyContainer a deriving Show

instance Functor MyContainer where
  fmap f (MyContainer a) = MyContainer (f a)
  
