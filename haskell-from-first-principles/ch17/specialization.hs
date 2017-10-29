x :: a -> [a]
x = pure
y :: [a -> b] -> [a] -> [b]
y = (<*>)

z :: a -> IO a
z = pure
w :: IO (a -> b) -> IO a -> IO b
w = (<*>)

u :: Monoid a => b -> (a, b)
u = pure
v :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
v = (<*>)

f :: b -> a -> b
f = pure
g :: (a -> b -> c) -> (a -> b) -> (a -> c)
g = (<*>)

-- it then turns out that for this applicative
-- (f <*> g <*> h <*> v) z = f z (g z) (h z) (v z)
-- see
-- https://stackoverflow.com/questions/46992307/an-intuitive-idea-of-the-arrow-operator-applicative

-- a small test
t = ((\x y z w -> [x, y, z, w]) <*> (1+) <*> (2+) <*> (3+)) 4
