import Data.Semigroup

newtype Identity a = Identity a
instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

data Two a b = Two a b
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x1 y1 <> Two x2 y2 = Two (x1 <> x2) (y1 <> y2)

newtype BoolConj = BoolConj Bool deriving Show
instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)
  
newtype BoolDisj = BoolDisj Bool deriving Show
instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

data Or a b = Fst a | Snd b deriving Show
instance Semigroup (Or a b) where
  Fst f1 <> Fst f2 = Fst f2
  Fst f <> Snd s = Snd s
  Snd s <> _ = Snd s
  
