import Control.Monad.Trans.Reader

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> putStrLn ("Hi: " ++ show r) >> pure (r + 1)

foo :: IO Int
foo = runReaderT rPrintAndInc 1
