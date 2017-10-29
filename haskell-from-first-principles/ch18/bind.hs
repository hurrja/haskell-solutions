import Control.Monad (join)
bind :: Monad m => m a -> (a -> m b) -> m b
bind x f = join $ fmap f x
