myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldr (\ x acc -> p x && acc) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\ x acc -> p x || acc) False

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x : xs)
  | p x = x : myTakeWhile p xs
  | otherwise = []
  
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile p (x : xs)
  | p x = myDropWhile p xs
  | otherwise = (x : xs)
  
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\ x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\ x acc -> if p x then x : acc else acc) []

dec2int :: [Int] -> Int
dec2int = foldl (\ acc x -> 10 * acc + x) 0

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \ x y -> f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f = \ (x, y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)
  
chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

myUFMap :: (a -> b) -> [a] -> [b]
myUFMap f = unfold null (f . head) tail

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (\ _ -> False) id f
