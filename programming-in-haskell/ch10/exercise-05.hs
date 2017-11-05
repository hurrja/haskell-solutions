adder :: IO ()
adder = readCount >>= readNumbers >>= writeSum
  where
    readNumber :: IO Int
    readNumber = getLine >>= (\s -> pure (read s :: Int))

    readCount :: IO Int
    readCount = putStr "How many numbers? " >> readNumber
      
    readNumbers :: Int -> IO [Int]
    readNumbers n = sequence $ replicate n readNumber

    writeSum :: [Int] -> IO ()
    writeSum xs = putStr "The total is " >> (putStrLn $ show $ sum xs)
