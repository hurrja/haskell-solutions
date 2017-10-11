import Data.Char
import Data.List
import Data.Maybe

type NumAlphs = Int
data Button = NumOnly Int | NumAlph Int NumAlphs | NumSpecial Int [Char] | Special [Char]
  deriving (Eq, Show)
data Phone = Keyboard [Button] deriving (Eq, Show)

buttons :: Phone -> [Button]
buttons (Keyboard bs) = bs

thePhone :: Phone
thePhone = Keyboard [NumOnly 1,
                     NumAlph 2 3,
                     NumAlph 3 3,
                     NumAlph 4 3,
                     NumAlph 5 3,
                     NumAlph 6 3,
                     NumAlph 7 4,
                     NumAlph 8 3,
                     NumAlph 9 4,
                     Special "*^",
                     NumSpecial 0 "+_",
                     Special "#.,"]

keyboardChars :: Phone -> [[Char]]
keyboardChars = (go alphas) . buttons
  where
    alphas = ['a'..'z']
    go :: [Char] -> [Button] -> [[Char]]
    go _ [] = []
    go cs ((NumOnly _):bs) = "" : go cs bs
    go cs ((NumAlph _ n):bs) = take n cs : go (drop n cs) bs
    go cs ((Special chars):bs) = chars : go cs bs 
    go cs ((NumSpecial _ chars):bs) = chars : go cs bs

capsButtons :: Phone -> Maybe [(Button, Int)]
capsButtons = charToButtons '*'

charToButtons :: Char -> Phone -> Maybe [(Button, Int)]
charToButtons c phone = pure (++) <*> (if isUpper c then capsButtons phone else Just []) <*>
  (go (toLower c) $ zip (keyboardChars phone) (buttons phone))
  where
    go c [] = Nothing
    go c (p:ps)
      | ind == Nothing = go c ps
      | otherwise = Just [(snd p, (1+) $ fromJust ind)]
      where
        ind = elemIndex c (fst p)

-- stringToButtons :: String -> Phone -> Maybe [(Button, Int)]
stringToButtons str phone = map (\ c -> charToButtons c phone) str
