-- newtype with function to provide output and next state (state function 'sf')
newtype State s a = ST (s -> (a, s))

instance Functor (State s) where
  fmap f (ST sf) = ST $ \s -> let (y, ns) = sf s in (f y, ns)
  
instance Applicative (State s) where
  pure v = ST $ \s -> (v, s)
  (ST f) <*> (ST sf) = ST $ \s ->
    let
      (y, ns) = sf s
      (g, nns) = f ns
    in (g y, nns)
  
instance Monad (State s) where
  return = pure
  (ST sf) >>= f = ST $ \s -> let (v, ns) = sf s
                                 ST sf2 = f v
                              in sf2 ns
                          
