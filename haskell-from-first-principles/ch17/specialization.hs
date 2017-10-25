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
