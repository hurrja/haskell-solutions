import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- the actual exercise
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT (const (pure (Right (Just 1))))

-- testing with simpler version
emb :: (ExceptT String (ReaderT () IO)) Int
emb = ExceptT (ReaderT (const (pure $ Right 1)))

-- testing reader
bar :: ReaderT String (MaybeT []) Int
bar = pure 1

foo :: ReaderT String (MaybeT []) Int
foo = ReaderT $ const (MaybeT $ [Just 1])

-- more testing with monad transform combos
g :: MaybeT (MaybeT []) Int
g = MaybeT (MaybeT ([Just (Just 1)]))

h :: (ExceptT String (MaybeT [])) Int
h = ExceptT $ MaybeT $ [Just (Right 1)]


t :: MaybeT (ReaderT String []) Int
t = MaybeT (ReaderT $ const ([Just 1]))

