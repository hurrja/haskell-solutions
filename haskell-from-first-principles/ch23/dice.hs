newtype State s a = ST { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (ST rs) = ST $ \st -> let (y, ns) = rs st in (f y, ns)
  
instance Applicative (State s) where
  pure x = ST $ \st -> (x, st)
  (ST f) <*> (ST rs) = ST $ \st ->
    let
      (y, ns) = rs st
      (g, nns) = f ns
    in (g y, nns)
  
instance Monad (State s) where
  return = pure
  (ST rs) >>= f = ST $ \st -> let (x, ns) = rs st
                                  ST rs2 = f x
                              in rs2 ns
                          
