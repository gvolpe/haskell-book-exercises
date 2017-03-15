module PoemLines where

-- generic function for both myWords and myLines
splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs  = (takeWhile (/=c) xs) : splitOn c (drop 1 $ dropWhile (/=c) xs)

-- takeWhile & dropWhile first exercise
myWords :: String -> [String]
myWords xs = splitOn ' ' xs

-- poem sentences
firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> [String]
myLines xs = splitOn '\n' xs

-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
