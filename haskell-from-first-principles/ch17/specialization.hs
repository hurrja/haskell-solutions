-- had to do some diggin' around to figure out how to approach this problem
x = (pure :: a -> [a]) 1
y = ((<*>) :: [a -> b] -> [a] -> [b]) [(+1)] [1..2]

z = (pure :: a -> IO a) "foo"
w = ((<*>) :: IO (a -> b) -> IO a -> IO b) (pure ("bar" ++)) $ pure "foo"
