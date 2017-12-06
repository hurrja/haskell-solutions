sumSquares :: (Enum a, Num a) => a
sumSquares = sum [ x^2 | x <- [1..100] ]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square n = grid n n

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = take n [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = take n [x | x <- [1..], x == (sum . init . factors) x]
  where factors x = [m | m <- [1..x], mod x m == 0]

coords :: Num a => [(a, a)]
coords = [(x, y) | x <- [1, 2], y <- [3, 4]]
coords' :: Num a => [(a, a)]
coords' = concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]

positions :: Eq a => a -> [a] -> [Int]
positions v xs = find v $ zip xs [0..(length xs - 1)]
  where
    find :: Eq a => a -> [(a, b)] -> [b]
    find k t = [v | (k', v) <- t, k' == k]

scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
