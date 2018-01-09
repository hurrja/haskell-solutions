import qualified System.Environment as Sys (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile =
  readFile inputFile >>= \inStr -> writeFile outputFile (function inStr)

main :: IO ()
main = mainWith myFunction
  where
    mainWith :: (String -> String) -> IO ()
    mainWith function = do
          args <- Sys.getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

    -- replace "id" with the name of our function below
    myFunction = id

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []
  where
    isLineTerminator :: Char -> Bool
    isLineTerminator c = c == '\r' || c == '\n'

