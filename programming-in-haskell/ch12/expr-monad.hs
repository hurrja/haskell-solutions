-- instances
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  fmap f (Var x) = Var $ f x
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
  fmap _ (Val x) = Val x

instance Applicative Expr where
  pure x = Var x
  (Val x) <*> _ = Val x
  (Var f) <*> e = f <$> e
  (Add f g) <*> e = Add (f <*> e) (g <*> e)

instance Monad Expr where
  return = pure
  (Val x) >>= _ = Val x
  (Var x) >>= f = f x
  (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)

-- example of application: substituting variables

expr :: Expr Char
expr = Add (Add (Var 'x') (Var 'y')) (Add (Var 'x') (Val 1))

substXy :: Char -> String
substXy c 
  | c == 'x' = "t"
  | c == 'y' = "v"
  | otherwise = ""

substDo :: (Expr Char) -> (Expr String)
substDo e = do
  v <- e
  pure $ substXy v

-- same without do
subst :: (Expr Char) -> (Expr String)
subst e = e >>= (\v -> pure $ substXy v)

-- same with applicative
substApp :: (Expr Char) -> (Expr String)
substApp e = pure substXy <*> e

-- application: substituting an integer value for a variable
eval :: (Expr Char) -> (Expr Int)
eval e = do
  v <- e
  Val (if v == 'x' then 3 else 0)
  
foo :: (Expr Char) -> (Expr Char) -> (Expr String)
foo e1 e2 = do
  v1 <- e1
  v2 <- e2
  pure $ v1 : '_' : v2 : []
