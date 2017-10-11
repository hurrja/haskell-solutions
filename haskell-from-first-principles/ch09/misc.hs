myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo f t
  | fromEnum f > fromEnum t = []
  | otherwise = ft f t
  where
    ft f t
      | fromEnum f == fromEnum t = [t]
      | otherwise = f : (ft (succ f) t)
      

--

splitSentence :: String -> Char -> [String]
splitSentence s c
  | s == "" = []
  | otherwise = takeWhile notChar s
                : (splitSentence (dropWhile char (dropWhile notChar s)) c)
    where
      char = (c ==)
      notChar = not . char 

--

mySqr = [ x^2 | x <- [1 .. 5]]
myCube = [ y^3 | y <- [1 .. 5]]

--

foo = [1, undefined, 3]
myLength [] = 0
myLength (_:xs) = 1 + length xs

--

itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

--

multiples3 = filter (\ x -> rem x 3 == 0)
numMultiples3 = length . multiples3
removeArticles = filter (\ w -> w /= "the" && w /= "a" && w /= "an") . words

--

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _  = []
myZip (a : as) (b : bs) = (a, b) : (myZip as bs)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (a : as) (b : bs) = (f a b) : (myZipWith f as bs)

myZip2 = myZipWith (\ a b -> (a, b))

--

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs)
  | x = True
  | otherwise = myOr xs
  
myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = myOr (map f l)

myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny ((==) e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myBy _ _ [x] = x
myBy ord f (x : xs)
  | (f x rest) == ord = x
  | otherwise = rest
  where rest = myBy ord f xs

myMaximumBy = myBy GT
myMinimumBy = myBy LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
