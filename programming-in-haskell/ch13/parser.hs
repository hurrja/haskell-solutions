module Parser where

newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (P p) = P (\s -> fmap (\(v, r) -> (f v, r)) $ p s)

