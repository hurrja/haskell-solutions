import Control.Monad.Trans.State

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> let str = show s in
  putStrLn ("Hi: " ++ str) >> pure (str, s + 1)
