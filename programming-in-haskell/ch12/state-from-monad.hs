-- definining other instances from monad

type State = Int
newtype ST a = S (State -> (a, State))
app :: ST a -> State -> (a, State)
app (S st) = st

instance Monad ST where
  st >>= f = S (\s ->
                  let (x, s') = app st s in app (f x) s')
  return = pure

instance Applicative ST where
  pure x = S (\s -> (x, s))
  stf <*> stx = do
    v <- stx
    f <- stf
    pure $ f v
  -- same without do
  -- stf <*> stx = stx >>= (\v -> stf >>= (\f -> pure $ f v))

instance Functor ST where
  fmap g st = do
    v <- st
    pure $ g v
  -- same without do
  -- fmap g st = st >>= (\v -> pure $ g v)
