-- this works but is devious to test, because I would have to set stty properties first
-- a cheap way to test now is to first compile with ghc and then run
-- stty -echo; ./exercise-06; stty echo
-- this will not echo keyboard commands, but you can read correct behaviour from two output lines

main :: IO ()
main = readLine >>= putStrLn

readLine :: IO String
readLine = go ""
  where
    go :: String -> IO String
    go acc = do
      c <- getChar
      case c of
        '\n' -> putChar c >> pure acc
        '\DEL' -> if not (null acc)
                  then
                    putChar '\b' >> (go $ init acc)
                  else
                    go acc
        _ -> putChar c >> (go $ acc ++ [c])
        
