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

                          
