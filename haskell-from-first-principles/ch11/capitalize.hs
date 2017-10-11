import Data.Char

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . (map capitalizeWord) . sentences

sentences :: String -> [String]
sentences [] = []
sentences str = sent : sentences rest
  where
    (sent, rest) = splitAtPoint str

splitAtPoint :: String -> (String, String)
splitAtPoint str = (start ++ (takeWhile pointOrSpace end), dropWhile pointOrSpace end)
  where
    pointOrSpace = (\ c -> (c == '.') || (c == ' '))
    notPoint = (/= '.')
    start = takeWhile notPoint str
    end = dropWhile notPoint str
    
