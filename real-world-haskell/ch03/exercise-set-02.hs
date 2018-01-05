-- exercise 8
data Tree a = Empty | Node a (Tree a) (Tree a)

height :: (Integral b) => Tree a -> b
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

myTree :: Tree Int
myTree = Node 1 (Node 2 Empty Empty) Empty

-- exercise 9
data Direction = ToLeft | ToRight | Collinear deriving Show

-- exercise 10
data Point a = Point { x :: a, y :: a }
diff :: Num a => Point a -> Point a -> Point a
diff q p = Point (x q - x p) (y q - y p)

dir :: (Num a, Ord a) => Point a -> Point a -> Point a -> Direction
dir p q r 
  | crVal == 0 = Collinear
  | crVal > 0 = ToLeft
  | otherwise = ToRight
  where pq = diff q p
        qr = diff r q
        crossK :: Num a => Point a -> Point a -> a
        crossK u v = x u * y v - y u * x v
        crVal = crossK pq qr
                
              
          
