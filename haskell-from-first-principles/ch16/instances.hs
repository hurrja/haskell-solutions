data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  
data K a b = K a deriving Show
instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype FlippedK a b = FlippedK (Flip K a b) deriving Show
instance Functor (FlippedK a) where
  fmap f (FlippedK (Flip (K a))) = FlippedK (Flip (K $ f a))

myK = K "foo"
myFlippedK = FlippedK (Flip (K "foo"))
