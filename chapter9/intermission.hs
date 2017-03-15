myEnumFromTo :: (Ord a, Enum a) => a-> a -> [a]
myEnumFromTo x y
  | x < y     = x : myEnumFromTo (succ x) y
  | otherwise = [] 

-- I don't get the requirement for these 4 functions... I can only think about class constraint to make it work.
eftBool :: Bool -> Bool -> [Bool]
eftBool x y = x : y : [] 

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = undefined

eftInt :: Int -> Int -> [Int]
eftInt = undefined

eftChar :: Char -> Char -> [Char]
eftChar = undefined

-- list comprehensions
mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

one = [(x,y) | x <- mySqr, y <- myCube]
two = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
three = (length one) - (length two)

-- filter
fOne xs = filter (\x -> rem x 3 == 0) xs
fTwo    = length . fOne

myFilter xs = filter (\x -> x/="the" && x/="an" && x/="a") $ words xs

-- zip
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _ []          = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a,b)]
myZip' = myZipWith (,)
