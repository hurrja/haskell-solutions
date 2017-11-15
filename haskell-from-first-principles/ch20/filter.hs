import Data.Bool (bool)

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF p = foldMap (\v -> if p v then pure v else mempty)
