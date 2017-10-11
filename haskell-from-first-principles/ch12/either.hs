lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left v) = (:) v
    f _ = id
    
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Right v) = (:) v
    f _ = id
    
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right r) = Just $ f r
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left v) = f v
either' _ f (Right v) = f v

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\v -> Just $ f v)
