import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Only (mempty)
  mappend (Only x) (Only y) = Only (mappend x y)
  mappend Nada m = m
  mappend m Nada = m
