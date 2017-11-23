import Data.Char

cap :: String -> String
cap xs = map toUpper xs

rev :: String -> String
rev xs = reverse xs

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

apped :: String -> String
apped = pure cap <*> rev

tupled :: String -> (String, String)
tupled = pure (,) <*> cap <*> rev

tupledMonad :: String -> (String, String)
tupledMonad = do
  x <- cap
  y <- rev
  pure (x, y)

