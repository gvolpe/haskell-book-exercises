module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "not a digit"

digits :: Int -> [Int]
digits n = loop n []
  where loop x acc
         | x < 10    = x : acc
         | otherwise = loop (div x 10) ((mod x 10) : acc)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord $ digits n)
