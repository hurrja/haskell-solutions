module Jammin where
import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Ord, Show)
data JamJars = Jam {
  fruit :: Fruit,
  jars :: Int}
  deriving (Eq, Ord, Show)

cupboard = [row1, row2, row3, row4, row5]
  where
    row1 = Jam Blackberry 5
    row2 = Jam Apple 15
    row3 = Jam Peach 3
    row4 = Jam Plum 9
    row5 = Jam Blackberry 11

numJars :: [JamJars] -> Int
numJars = sum . map jars

bestRow :: [JamJars] -> JamJars
bestRow jarRows = foldl
  (\ acc row -> if jars acc >= jars row then acc else row)
  (head jarRows)
  (tail jarRows)

groupJams :: [JamJars] -> [[JamJars]]
groupJams = groupBy (\ jam1 jam2 -> (fruit jam1) == (fruit jam2)) . sortBy compare
