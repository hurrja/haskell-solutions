factorial :: Integer -> Integer
factorial n
  | n < 0 = undefined
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)

sumdown :: Integer -> Integer
sumdown n
  | n < 0 = undefined
  | n <= 1 = n
  | otherwise = n + sumdown (n - 1)

pow :: Int -> Int -> Int
pow a b
  | b == 0 = 1
  | even b = pow (a * a) (div b 2)
  | otherwise =  a * pow a (b - 1)

euclid :: Int -> Int -> Int
euclid m n
  | m == n = m
  | m > n = euclid (m - n) n
  | otherwise = euclid m (n - m)
  
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs : xss) = xs ++ myConcat xss

myReplicate :: Int -> a -> [a]
myReplicate n x
  | n <= 0 = []
  | otherwise = x : replicate (n - 1) x

mySelect :: [a] -> Int -> a
mySelect xs n
  | n < 0 = undefined
  | n == 0 = head xs
  | otherwise = mySelect (tail xs) (n - 1)
  
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = e == x || myElem e xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ fst s) (msort $ snd s)
  where
    s = halve xs
    halve :: [a] -> ([a], [a])
    halve [] = ([], [])
    halve [x] = ([x], [])
    halve (x : y : xs) = (x : fst h, y : snd h)
      where h = halve xs
      
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (x : xs) = last xs

