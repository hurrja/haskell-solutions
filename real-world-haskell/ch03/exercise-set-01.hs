data List a = Nil | Cons a (List a) deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

data Tree a = Node (Maybe a) (Maybe (Tree a)) (Maybe (Tree a)) deriving Show

-- example uses
myTree :: Tree Int
myTree = Node (Just 3) (Just (Node (Just 2) Nothing Nothing)) Nothing

myTree' :: Tree Int
myTree' = Node Nothing Nothing Nothing
