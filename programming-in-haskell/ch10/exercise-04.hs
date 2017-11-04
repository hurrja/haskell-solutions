adder :: IO ()
adder = readCount >>= readNumbers >>= writeSum
  where
    readCount :: IO Int
    readCount = do
      putStr "How many numbers? "
      countStr <- getLine
      pure $ (read countStr :: Int)
      
    readNumbers :: Int -> IO [Int]
    readNumbers 0 = pure []
    readNumbers n = do
      numStr <- getLine
      pure (read numStr :: Int) >>= (\num -> fmap ((:) num) $ readNumbers $ n - 1)

    writeSum :: [Int] -> IO ()
    writeSum xs = do
      putStr "The total is "
      putStrLn $ show $ sum xs
      
