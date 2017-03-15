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
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- Generic for filter db 
filterDb :: (DatabaseItem -> [a]) -> [DatabaseItem] -> [a]
filterDb _ [] = []
filterDb f xs = foldr (\ x y -> f x ++ y) [] xs
--filterDb f xs = concat $ map f xs -- SAME RESULT

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = filterDb onlyTime

onlyTime :: DatabaseItem -> [UTCTime]
onlyTime x = case x of 
  DbDate time -> time : []
  _           -> []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = filterDb onlyNumber

onlyNumber :: DatabaseItem -> [Integer]
onlyNumber x = case x of
  DbNumber number -> number : []
  _               -> []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = avg $ map fromInteger $ filterDbNumber xs

avg :: Fractional a => [a] -> a
avg xs = (sum xs) / fromIntegral (length xs)
