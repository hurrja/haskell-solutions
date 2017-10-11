import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]

theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 8
  , DbNumber 11
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr appendIfDate []
  where
    appendIfDate (DbDate t) lst = t : lst
    appendIfDate _ lst = lst

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr appendIfNum []
  where
    appendIfNum (DbNumber n) lst = n : lst
    appendIfNum _ lst = lst
                           
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db =
  let nums = filterDbNumber db in
    (fromIntegral $ sum nums) / (fromIntegral $ length nums)
