import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT (const (pure (Right (Just 1))))

emb :: (ExceptT String (ReaderT () IO)) Int
emb = ExceptT (ReaderT (const (pure $ Right 1)))

bar :: ReaderT String (MaybeT []) Int
bar = pure 1

g :: MaybeT (MaybeT []) Int
g = MaybeT (MaybeT ([Just (Just 1)]))

h :: (ExceptT String (MaybeT [])) Int
h = ExceptT $ MaybeT $ [Just (Right 1)]

foo :: ReaderT String (MaybeT []) Int
foo = ReaderT $ const (MaybeT $ [Just 1])

t :: MaybeT (ReaderT String []) Int
t = MaybeT (ReaderT $ const ([Just 1]))

