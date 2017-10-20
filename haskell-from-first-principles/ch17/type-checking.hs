import Control.Applicative
import Data.List (elemIndex)

lookupLst = zip [1..3] [4..6]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ lookupLst)
-- or
added' :: Maybe Integer
added' = pure (+3) <*> (lookup 3 $ lookupLst)

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
  where
    y :: Maybe Integer
    y = lookup 3 lookupLst
    z :: Maybe Integer
    z = lookup 2 lookupLst
-- or
-- tupled' :: Maybe (Integer, Integer)
-- tupled' = liftA2 (,) y z

max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x <*> y
  where
    lst = [1..5]
    x :: Maybe Int
    x = elemIndex 3 lst
    y :: Maybe Int
    y = elemIndex 4 lst
-- or
-- maxed' :: Maybe Int
-- maxed = pure max' <*> x <*> y

-- this one is sort of weird, because sum of pair sums up just the
-- second element
summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)
  where
    xs = [1..3]
    ys = [4..6]
    lst = zip xs ys
    x :: Maybe Integer
    x = lookup 3 lst
    y :: Maybe Integer
    y = lookup 2 lst
    
