-- Here I take a different approach from the one in the book, because
-- the book no longer explains all the concepts used. This is an
-- amalgamation of the books idea to use random number generation as
-- an example of state, and Hutton's approach to model state using
-- state transformers. Here I have implemented both an applicative
-- case and a monad case, where in the monad case the past of casts is
-- used.

import System.Random (StdGen, mkStdGen, next)

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
cast n = runStTrans $ foldr (<*>) (pure []) $ replicate n addCast
  where
    addCast :: StTrans DieState ([Int]->[Int])
    addCast = ST $ \s -> let (v, ns) = next s
                             d = mod v 6 + 1
                         in ((d:), ns)


-- monad case: cast a die n times, producing a list where consecutive
-- casts have different values
castDiff :: Int -> DieState -> ([Int], DieState)
castDiff n = runStTrans $ foldl (>>=) (pure []) $ replicate n newCast
  where
    newCast :: [Int] -> StTrans DieState [Int]
    newCast past = ST $ \s -> let (v, ns) = next s
                                  d = case past of
                                        [] -> mod v 6 + 1
                                        _ -> exclude (head past) (mod v 5)
                              in (d : past, ns)
      where
        -- maps v in [0..4] to a value from [1..6] that excludes x
        -- example:
        -- λ> map (exclude 3) [0..4] 
        -- [4,5,6,1,2]
        exclude :: Int -> Int -> Int
        exclude x v = (v + x) `mod` 6 + 1
