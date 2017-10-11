mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

--

k (x, y) = x
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f = \ (a, b, c) (d, e, f) -> ((a, d), (c, f))

--

functionC :: Ord a => a -> a -> a
functionC x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n
  
nums :: (Num a, Ord a, Num b) => a -> b
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

--

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1
oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

numbers :: (Ord a, Num a, Num b) => a -> b
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1
  
--

fFunc :: Ord a => a -> a -> Bool
fFunc x y = x > y

--

-- doesn't work for k = 0
nthDigit :: Integral a => a -> Int -> a
nthDigit x k = snd . f . fst . f $ x
  where f = flip divMod $ 10^k
        
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y t = case t of
  True -> x
  False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y t
  | t = x
  | otherwise = y
  
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f (a), c)

