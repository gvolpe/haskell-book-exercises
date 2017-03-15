stops  = "pbtdkg"
vowels = "aeiou"

one = [(x,y,z) | x <- stops, y <- vowels, z <- stops]
two = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- avg
seekritFunc x = div (sum (map length (words x))) (length (words x))
avg x = sumLen / len
  where len    = fromIntegral $ length (words x)
        sumLen = fromIntegral $ sum $ map length (words x)

-- using folds
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\ a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\ a b -> x == a || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' x xs = any (==x) xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\ a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\ a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

