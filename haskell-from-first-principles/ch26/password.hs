import Control.Monad 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Class 

main :: IO ()
main = do 
  password <- runMaybeT getPassword
  case password of 
    Just _  -> putStrLn "valid password!"
    Nothing -> putStrLn "invalid password!"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT IO String 
getPassword = lift getLine >>= \pw -> guard (isValid pw) >> pure pw

