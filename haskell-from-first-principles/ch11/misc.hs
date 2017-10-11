-- dogs
data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- transportation

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini $ Price 14000
urCar :: Vehicle
urCar = Car Mazda $ Price 20000
clownCar :: Vehicle
clownCar = Car Tata $ Price 7000
doge :: Vehicle
doge = Plane PapuAir $ Size 120

isCar :: Vehicle -> Bool
isCar (Car _ _ ) = True
isCar _  = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m

-- TooMany

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (<) 42
  
data MyIntStr = MyIntStr Int String
instance TooMany MyIntStr where
  tooMany (MyIntStr i s) = tooMany i

data MyIntInt = MyIntInt Int Int
instance TooMany MyIntInt where
  tooMany (MyIntInt a b) = tooMany $ a + b

data MyNumToo a = MyNumToo a a
instance (Num a, TooMany a) => TooMany (MyNumToo a) where
  tooMany (MyNumToo a b) = tooMany $ a + b

