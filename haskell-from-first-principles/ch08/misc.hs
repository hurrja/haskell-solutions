cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--

cumSum :: (Eq a, Num a) => a -> a
cumSum x
  | x == 0 || x == 1 = x
  | otherwise = x + cumSum (x - 1)

recProd :: (Integral a) => a -> a -> a
recProd x y = sgn $ go (abs x) (abs y)
  where sgn
          | (x < 0 && y < 0) || (x > 0 && y > 0) = id
          | otherwise = negate
        go x y
          | x == 0 = 0
          | otherwise = y + go (x - 1) y
  
