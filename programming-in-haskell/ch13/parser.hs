module Parser where

newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (P p) = P (\s -> fmap (\(v, r) -> (f v, r)) $ p s)

instance Applicative Parser where
  pure v = P (\s -> Just (v, s))
  (P pf) <*> (P p) = P (\s -> case p s of
                                Nothing -> Nothing
                                Just (v, r) -> case pf r of
                                  Nothing -> Nothing
                                  Just (f, r2) -> Just (f v, r2))

instance Monad Parser where
  return = pure
  (P p) >>= f = P (\s -> case p s of
                      Nothing -> Nothing
                      Just (v, r) -> parse (f v) $ r)
                          
empty :: Parser Bool
empty = P $ (\s -> if null s then Just (True, s) else Nothing)
             
accept :: (Char -> Bool) -> Parser Char
accept p = P $ (\s -> case s of
                        [] -> Nothing
                        (c:cs) -> if p c then Just (c, cs) else Nothing)

