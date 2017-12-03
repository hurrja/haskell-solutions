import System.Random

-- newtype with function to provide value and next state
newtype StTrans s a = ST {runStTrans :: s -> (a, s)}

-- below, 's' refers to "state", 'ns' to "next state", 'v' to value,
-- and 'st' to state transformer

instance Functor (StTrans s) where
  fmap f (ST st) = ST $ \s -> let (v, ns) = st s in (f v, ns)
  
instance Applicative (StTrans s) where
  pure v = ST $ \s -> (v, s)
  (ST f) <*> (ST st) = ST $ \s ->
    let
      (v, ns) = st s
      (g, nns) = f ns
    in (g v, nns)
  
instance Monad (StTrans s) where
  return = pure
  (ST st) >>= f = ST $ \s -> let (v, ns) = st s
                                 ST st2 = f v
                             in st2 ns
                          
-- cast :: Int -> [Int]
-- cast seed = runStTrans (foldr ((<*>) doCast)
cast :: StTrans StdGen ([Int]->[Int])
cast = ST $ \s -> let (v, ns) = next s
                      d = mod v 6 + 1
                  in ((d:), ns)

