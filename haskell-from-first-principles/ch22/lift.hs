liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g x y = g <$> x <*> y
