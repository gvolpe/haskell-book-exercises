import Data.Char

filterUppercase :: String -> String
filterUppercase xs = filter isUpper xs

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs 

toUppercase :: String -> String
toUppercase []     = []
toUppercase (x:xs) = toUpper x : toUppercase xs

capitalizeHead :: String -> Maybe Char
capitalizeHead []     = Nothing
capitalizeHead (x:xs) = Just (toUpper x)

capitalizeHead' :: String -> Char
capitalizeHead' = toUpper . head

-- more

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = if x == y then True else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' x xs = any (==x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x : []

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs = squish $ map f xs

