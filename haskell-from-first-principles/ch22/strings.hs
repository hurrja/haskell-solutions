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

