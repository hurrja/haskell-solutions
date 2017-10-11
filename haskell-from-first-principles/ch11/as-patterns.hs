import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xs@(x:rest) (y:ys)
  | x == y = isSubsequenceOf rest ys
  | otherwise = isSubsequenceOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\ w@(c:cs) -> (w, toUpper c : cs)) . words
