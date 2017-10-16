import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, mempty, mappend)

newtype Identity a = Identity a
instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)
instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

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
  
newtype Combine a b = Combine { unCombine :: (a -> b) }
instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \n -> f n <> g n

newtype Comp a = Comp { unComp :: a -> a}
instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g
  
data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  Success s1 <> Success s2 = Success s2
  Success s <> Failure f = Failure f
  Failure f <> Success s = Failure f
  Failure f1 <> Failure f2 = Failure $ f1 <> f2
  
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success s1) <> AccumulateRight (Success s2) =
    AccumulateRight (Success $ s1 <> s2)
  AccumulateRight (Success s) <> AccumulateRight (Failure f) =
    AccumulateRight (Success s)
  AccumulateRight (Failure f) <> AccumulateRight (Success s) =
    AccumulateRight (Success s)
  AccumulateRight (Failure f1) <> AccumulateRight (Failure f2) =
    AccumulateRight (Failure f2)
    
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure f1) <> AccumulateBoth (Failure f2) =
    AccumulateBoth (Failure $ f1 <> f2)
  AccumulateBoth (Success s1) <> AccumulateBoth (Success s2) =
    AccumulateBoth (Success $ s1 <> s2)
  AccumulateBoth (Failure f) <> AccumulateBoth (Success s) =
    AccumulateBoth (Success s)
  AccumulateBoth (Success s) <> AccumulateBoth (Failure f) =
    AccumulateBoth (Failure f)
