-- ex 1
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

svsConstr :: Eq a => [a] -> [a] -> a -> [[a]]
svsConstr s v c = filter ((==) c . (flip (!!) 0)) $ svs s v

svs :: [a] -> [a] -> [[a]]
svs s v = [ x : y : [z] | x <- s, y <- v, z <- s ]

nouns :: [String]
nouns = ["dog", "man", "car", "flashlight"]

verbs :: [String]
verbs = ["eats", "sees", "scares", "finds"]

-- ex 2

seekritFunc :: String -> Rational
seekritFunc x =
  let w = words x in
    (fromIntegral $ sum (map length w)) / (fromIntegral $ length w)
    
-- standard functions using fold
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = foldr ((||) . pred) False

myElem :: Eq a => a -> [a] -> Bool
myElem item = foldr ((||) . ((==) item)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' item = myAny (item == ) 

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\ x -> if pred x then ((:) x) else id) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squish' :: [[a]] -> [a]
squish' = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp lst = foldr foo (last lst) lst
  where foo = \ x acc -> if comp x acc == GT then x else acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp lst = foldr foo (last lst) lst
  where foo = \ x acc -> if comp x acc == LT then x else acc
