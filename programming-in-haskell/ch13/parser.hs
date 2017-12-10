module Parser where
import Control.Applicative
import Data.Char

newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f p = P $ \s -> fmap (\(v, r) -> (f v, r)) $ parse p s

instance Applicative Parser where
  pure v = P $ \s -> Just (v, s)
  pf <*> p = P (\s -> case parse pf s of
                        Nothing -> Nothing
                        Just (f, r) -> parse (fmap f p) r)
  
instance Monad Parser where
  return = pure
  p >>= f = P (\s -> case parse p s of
                       Nothing -> Nothing
                       Just (v, r) -> parse (f v) r)
                          
instance Alternative Parser where
  empty = P $ \_ -> Nothing
  pa <|> pb = P $ \s -> parse pa s <|> parse pb s

end :: Parser Bool
end = P $ \s -> if null s then Just (True, s) else Nothing
             
accept :: (Char -> Bool) -> Parser Char
accept p = P (\s -> case s of
                      [] -> Nothing
                      (c:cs) -> if p c then Just (c, cs) else Nothing)

acceptChar :: Char -> Parser Char
acceptChar c = accept (== c)

comma :: Parser Char
comma = acceptChar ','

dot :: Parser Char
dot = acceptChar '.'

whitespace :: Parser Char
whitespace = acceptChar '\t' <|> acceptChar ' '

newline :: Parser Bool
newline = do { _ <- acceptChar '\n'; pure True }

doubleQuote :: Parser Char
doubleQuote = acceptChar '"'

digit :: Parser Int
digit = do
  d <- accept isDigit
  pure (read (d:[]) :: Int)

natural :: Parser Int
natural = do
  d <- some digit
  pure $ foldl (\acc v -> 10*acc + v) 0 d

negInteger :: Parser Int
negInteger = do
    _ <- acceptChar '-'
    n <- natural
    pure (-n)

token :: Parser a -> Parser a
token p = do
  _ <- many whitespace
  t <- p
  _ <- many whitespace
  pure t

quotedString :: Parser String
quotedString = do
  _ <- doubleQuote
  str <- many (accept (/= '"'))
  _ <- doubleQuote
  pure str

integer :: Parser Int
integer = negInteger <|> natural

data CSVItem = CSVStr String | CSVInt Int deriving Show
csvItem :: Parser CSVItem
csvItem = token $ do { i <- integer; pure $ CSVInt i} <|> do { s <- quotedString; pure $ CSVStr s }
csvRow :: Parser [CSVItem]
csvRow = do { i <- csvItem; ((newline <|> end) >> pure [i]) <|> do {_ <- comma; r <- csvRow; pure $ i : r }}

csvParser :: Parser [[CSVItem]]
csvParser = (end >> pure []) <|> do { r <- csvRow; rs <- csvParser; pure $ r : rs }
