sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Num a => a -> a
triple x = x * 3

foo x =
  let y = x * 2
      z = x ^ 2
  in 2 * y * z

mult1 = x * y
  where x = 5
        y = 6

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y^2

waxOff x = triple x

data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

awesome = ["Papuchon", "curry", "Haskell"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == reverse x)

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

x = (+)
f xs = w `x` 1
  where w = length xs
  
g = \ x -> x

myCar = \ (x : xs) -> x

h (a, b) = a

