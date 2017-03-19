import Data.Char
import Data.List

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf a@(x:_) b@(y:_) = and $ foldr (\x acc -> elem x b : acc) [True] a

f a@(x:xs) = (a, toUpper x : xs)

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) 
  | x == ' '  = capitalizeWord xs
  | otherwise = toUpper x : xs 

-- generic function from module PoemLines
splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs  = (takeWhile (/=c) xs) : splitOn c (drop 1 $ dropWhile (/=c) xs)

capitalizeParagraph :: String -> String
capitalizeParagraph x = (concat $ intersperse ". " capitals) ++ "."
  where capitals  = map capitalizeWord separated
        separated = splitOn '.' x
