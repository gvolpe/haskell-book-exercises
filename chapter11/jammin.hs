module Jammin where

import Data.List (sortBy, groupBy)

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Ord, Show)

data JamJars = Jam { fruit :: Fruit, count :: Int } deriving (Eq, Ord, Show)

row1 = Jam Peach 5
row2 = Jam Plum 7
row3 = Jam Apple 3
row4 = Jam Blackberry 9
row5 = Jam Peach 2
row6 = Jam Plum 1
allJam = [row1, row2, row3, row4, row5, row6]

jamCount :: [JamJars] -> [Int]
jamCount = fmap count

jamSum :: [JamJars] -> Int
jamSum = foldr ((+) . count) 0

mostRow :: [JamJars] -> JamJars
mostRow = maximum

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\ a b -> compareKind a b == EQ)
