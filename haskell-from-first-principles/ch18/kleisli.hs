import Control.Monad (join)

kleisli :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
kleisli f g x = f x >>= g
