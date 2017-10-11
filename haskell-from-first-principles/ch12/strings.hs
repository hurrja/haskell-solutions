import Data.Maybe
import Data.List 

replaceThe :: String -> String
replaceThe = concat . intersperse " " . map (\w -> if w == Nothing then "a" else fromJust w) .
  map ignoreThe . words 
  where
    ignoreThe :: String -> Maybe String
    ignoreThe "the" = Nothing
    ignoreThe xs = Just xs

vowels = "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go :: [String] -> Integer
    go [] = 0
    go [x] = 0
    go (x:xs@(y:ys)) = (if x == "the" && elem (head y) vowels then 1 else 0) + go xs
    
countVowels :: String -> Int
countVowels = length . filter (flip elem $ vowels)

newtype Word' = Word' String deriving (Eq, Show)
validateWord :: String -> Maybe Word'
validateWord s = let v = countVowels s
                     c = (length s) - v
                 in
                   if v > c then Nothing else Just (Word' s)
                   
