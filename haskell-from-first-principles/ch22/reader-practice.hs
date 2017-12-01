module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1..3]
y = [4..6]
z = [7..9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y
ys :: Maybe Integer
ys = lookup 6 $ zip y z
zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print . and $ sequA 5
  print . fromMaybe [] $ sequA <$> s'
  print . fromMaybe False $ bolt <$> ys
  print . (fmap . fmap) bolt z' $ 2
  print . (liftA . liftA) bolt z' $ 2 -- alternative
  print . fmap bolt <$> z' $ 2 -- another alternative
