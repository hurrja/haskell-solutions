for :: a -> (a -> Bool) -> (a -> a) -> (a -> b) -> [b]
for val pred step f
  | not $ pred val = []
  | otherwise = (f val) : (for (step val) pred step f)
