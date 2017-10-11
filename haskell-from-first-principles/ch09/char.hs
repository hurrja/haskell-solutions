import Data.Char

filterCaps :: String -> String
filterCaps = filter isUpper

capString :: String -> String
capString [] = []
capString (x : xs) = toUpper x : (capString xs)

firstCap :: String -> Char
firstCap = toUpper . head
