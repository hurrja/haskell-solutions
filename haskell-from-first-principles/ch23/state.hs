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
                          
-- ----------------------------------------------------------------
-- the die casting code begins here

-- state type from System.Random
type DieState = StdGen
mkDieState :: Int -> DieState
mkDieState = mkStdGen

-- applicative case: cast a die n times, starting with initial state
cast :: Int -> DieState -> ([Int], DieState)
cast n = runStTrans $ foldr (<*>) (pure []) $ replicate n doCast
  where
    doCast :: StTrans DieState ([Int]->[Int])
    doCast = ST $ \s -> let (v, ns) = next s
                            d = mod v 6 + 1
                        in ((d:), ns)


-- monad case: cast a die n times, producing a (not strictly) increasing sequence
castInc :: Int -> DieState -> ([Int], DieState)
castInc = undefined
