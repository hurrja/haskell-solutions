import Control.Monad.Trans.Reader

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> pure (r - 1)

-- pointfree
rDec' :: Num a => Reader a a
rDec' = ReaderT $ pure . (subtract 1)
