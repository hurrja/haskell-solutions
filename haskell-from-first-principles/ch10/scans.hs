fibs :: [Integer]
fibs = takeWhile (< 100) $ 0 : scanl (+) 1 fibs

facts :: [Integer]
facts = take 10 $ scanl (*) 1 [1..]
