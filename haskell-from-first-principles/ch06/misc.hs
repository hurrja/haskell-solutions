import Data.List

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
   (==) (TisAn x) (TisAn y) = x == y
  
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (Two a b) == (Two c d) = (a == c) && (b == d)
  
data StringOrInt =
  TIsAnInt Int |
  TIsAString String
instance Eq StringOrInt where
  (TIsAnInt _) == (TIsAString _) = False
  (TIsAString _) == (TIsAnInt _) = False
  (TIsAnInt i) == (TIsAnInt j) = i == j
  (TIsAString s) == (TIsAString r) = s == r

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (Pair x y) == (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple x y) == (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  (ThisOne _) == (ThatOne _) = False
  (ThatOne _) == (ThisOne _) = False
  (ThisOne x) == (ThisOne y) = x == y
  (ThatOne x) == (ThatOne y) = x == y
  
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello _) == (Goodbye _) = False
  (Goodbye _) == (Hello _) = False
  (Hello x) == (Hello y) = x == y
  (Goodbye x) == (Goodbye y) = x == y

--

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Show, Eq)
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

truth = Papu (Rocks "chases") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

--

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = \ f a b -> (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a

