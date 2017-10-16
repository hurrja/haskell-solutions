a = head (fmap (+1) $ read "[1]" :: [Int])
b = (fmap . fmap) (++ "lol") $ Just ["Hi,", "Hello"]
c = (*2) . (\x -> x - 2)
d = ((pure '1' ++) . show) . (\x -> [x, 1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi :: IO Integer
    in
      fmap (*3) changed
