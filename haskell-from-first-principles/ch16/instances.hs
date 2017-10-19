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

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
  
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
  
data List a = Nil | Cons a (List a)
instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat g) = OneGoat $ f g
  fmap f (MoreGoats l c r) = MoreGoats (fmap f l) (fmap f c) (fmap f r)
  
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor (TalkToMe) where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str $ f x
  fmap f (Read g) = Read $ f . g
