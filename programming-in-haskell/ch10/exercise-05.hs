adder :: IO ()
adder = readCount >>= readNumbers  >>= writeSum
  where
    readCount :: IO Int
    readCount = do
      putStr "How many numbers? "
      countStr <- getLine
      pure $ (read countStr :: Int)
      
    readNumbers :: Int -> IO [Int]
    readNumbers = sequence . go
      where
        go :: Int -> [IO Int]
        go 0 = []
        go n = (getLine >>= (\s -> pure (read s :: Int))) : (go $ n - 1)

    writeSum :: [Int] -> IO ()
    writeSum xs = do
      putStr "The total is "
      putStrLn $ show $ sum xs
      
