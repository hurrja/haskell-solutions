{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r

-- two different readers, both will read an int, the first one will
-- filter a list, keeping only the int values, the second one will
-- take elements from the beginning of a list that are smaller than
-- the given int
filterReader :: [Int] -> Int -> [Int]
filterReader lst = \i -> filter (== i) lst

takeReader :: [Int] -> Int -> [Int]
takeReader lst = \i -> takeWhile (< i) lst

-- versions using the builtin ((->) r) monad
useReadersDo :: Int -> ([Int], [Int])
useReadersDo = let lst = [1..10] in
  do
    l1 <- filterReader lst
    l2 <- takeReader lst
    pure (l1, l2)
  
useReadersBind :: Int -> ([Int], [Int])
useReadersBind = let lst = [1..10] in
  filterReader lst >>= (\l1 -> takeReader lst >>= (\l2 -> pure (l1, l2)))

-- version using the self-defined Reader as an applicative
useReadersApp :: Reader Int ([Int], [Int])
useReadersApp = let lst = [1..10] in
  pure (,) <*> Reader (filterReader lst) <*> Reader (takeReader lst)

-- version using the self-defined Reader as a monad
useReadersMon :: Reader Int ([Int], [Int])
useReadersMon = let lst = [1..10] in
  do
    l1 <- Reader (filterReader lst)
    l2 <- Reader (takeReader lst)
    pure (l1, l2)
