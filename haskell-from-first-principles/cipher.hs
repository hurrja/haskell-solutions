module Cipher where

import Data.Char (ord, chr)

shift :: Int -> Char -> Char
shift n = chr . intShift . ord
  where
    mapPair :: (a -> b) -> (a, a) -> (b, b)
    mapPair f (x, y) = (f x, f y)
    charLim :: (Int, Int)
    charLim = mapPair ord ('a', 'z')
    charMin :: Int
    charMin = fst charLim
    modN :: Int
    modN =  (snd charLim) - charMin + 1
    space :: Int
    space = ord ' '
    intShift :: Int -> Int
    intShift i
      | i == space = i
      | otherwise = (mod (i - charMin + n) modN) + charMin
    
caesar :: Int -> String -> String
caesar n msg = map (shift n) msg

unCaesar :: Int -> String -> String
unCaesar n msg = map (shift (-n)) msg

shiftBy :: String -> String -> Bool -> String
shiftBy key msg forward = zipWith (shift . (if forward then id else negate)) shifts msg
  where
    shifts = map ord key ++ shifts

vigenere :: String -> String -> String
vigenere key msg = shiftBy key msg True

unVigenere :: String -> String -> String
unVigenere key msg = shiftBy key msg False

