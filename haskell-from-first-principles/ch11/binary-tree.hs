module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert v Leaf = Node Leaf v Leaf
insert v (Node left a right)
  | v == a = Node left a right
  | v < a = Node (insert v left) a right
  | otherwise = Node left a (insert v right)
  
mapTree :: (a -> b) -> (BinaryTree a) -> (BinaryTree b)
mapTree _ Leaf = Leaf
mapTree f (Node left v right) = Node (mapTree f left) (f v) (mapTree f right)

preorder :: (BinaryTree a) -> [a]
preorder Leaf = []
preorder (Node left v right) = v : ((preorder left) ++ (preorder right))

inorder :: (BinaryTree a) -> [a]
inorder Leaf = []
inorder (Node left v right) = (inorder left) ++ [v] ++ (inorder right)

postorder :: Ord a => BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left v right) = (postorder left) ++ (postorder right) ++ [v]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left v right) = foldTree f (foldTree f (f v acc) left) right

testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

