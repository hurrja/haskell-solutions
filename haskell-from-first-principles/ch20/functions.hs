import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (flip (||) . (x ==)) False

minimum' :: (Foldable t, Ord a) => t a -> a
minimum' xs = case foldr f Nothing xs of
  Just x -> x
  Nothing -> error "issue"
  where
    f :: Ord a => a -> Maybe a -> Maybe a
    f x Nothing = Just x
    f x (Just y) = Just $ min x y

maximum' :: (Foldable t, Ord a) => t a -> a
maximum' xs = case foldr f Nothing xs of
  Just x -> x
  Nothing -> error "issue"
  where
    f :: Ord a => a -> Maybe a -> Maybe a
    f x Nothing = Just x
    f x (Just y) = Just $ max x y

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (const (1+)) 0 

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty
