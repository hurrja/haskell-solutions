{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
  
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName,
                       dogName :: DogName,
                       address :: Address } deriving (Eq, Show)
data Dog = Dog { dogsName :: DogName, dogsAddress :: Address } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")
chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDogR :: Reader Person Dog
getDogR = pure Dog <*> Reader dogName <*> Reader address

getDogA :: Person -> Dog
getDogA = pure Dog <*> dogName <*> address
