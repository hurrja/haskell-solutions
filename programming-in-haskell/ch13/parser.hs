module Parser where
import Control.Applicative

newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (P p) = P (\s -> fmap (\(v, r) -> (f v, r)) $ p s)

instance Applicative Parser where
  pure v = P $ \s -> Just (v, s)
  (P pf) <*> (P p) = P (\s -> case pf s of
                                Nothing -> Nothing
                                Just (f, r) -> case p r of
                                  Nothing -> Nothing
                                  Just (v, r2) -> Just (f v, r2))
  
instance Monad Parser where
  return = pure
  (P p) >>= f = P (\s -> case p s of
                      Nothing -> Nothing
                      Just (v, r) -> parse (f v) $ r)
                          
instance Alternative Parser where
  empty = P $ \_ -> Nothing
  (P pa) <|> (P pb) = P $ \s -> pa s <|> pb s

empty :: Parser Bool
empty = P $ \s -> if null s then Just (True, s) else Nothing
             
accept :: (Char -> Bool) -> Parser Char
accept p = P (\s -> case s of
                      [] -> Nothing
                      (c:cs) -> if p c then Just (c, cs) else Nothing)

comma :: Parser Char
comma = accept (== ',')

whitespace :: Parser Char
whitespace = accept $ \c -> (c == '\t') || (c == ' ')

newline :: Parser Char
newline = accept (== '\n')

doubleQuote :: Parser Char
doubleQuote = accept (== '"')

data CSVItem = CSVString String | CSVInteger Integer
csvRow :: Parser [CSVItem]
csvRow = undefined

csvParser :: Parser [[CSVItem]]
csvParser = undefined
