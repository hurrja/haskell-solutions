f :: a -> a -> a -> a
f = undefined

g :: a -> b -> c -> b
g = undefined

h :: (Num a, Num b) => a -> b -> b
h = undefined

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

l :: a -> a -> a
l x y = x

k :: a -> a -> a
k x y = y

m :: a -> b -> b
m x y = y

x = 5
y = x + 5
w = y * 10
z y = y * 10
ff = 4 / y

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = \ x -> f $ g x 

a :: (a -> c) -> a -> a
a = \ f x -> x

a' :: (a -> b) -> a -> b
a' = \ f x -> f x
