import Data.List (unfoldr)

myIterate :: (a -> a) -> a -> [a]
myIterate f v = v : (myIterate f $ f v)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f v =
  case f v of
    Nothing -> []
    Just (out, nextV) -> out : myUnfoldr f nextV
    
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\v -> Just (v, f v))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f v =
  case f v of
    Nothing -> Leaf
    Just (l, v, r) -> Node (unfold f l) v (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold
  (\v -> if v < n
         then
           let nextV = v + 1
           in Just (nextV, v, nextV)
         else
           Nothing
  )
  0
