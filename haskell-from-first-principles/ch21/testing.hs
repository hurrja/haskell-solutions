safeDiv :: Integral a => a -> a -> Maybe a
safeDiv x y = if y == 0 then Nothing else Just $ div x y

g = safeDiv 10

h :: Maybe ([Integer]) -> String
h = undefined
