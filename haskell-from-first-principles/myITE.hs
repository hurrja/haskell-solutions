import Data.Bool

f xs = map (\x -> bool x (-x) (x == 3)) xs
