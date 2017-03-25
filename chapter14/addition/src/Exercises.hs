module Exercises where

import Data.List (sort)

import Test.Hspec
import Test.QuickCheck

half x = x / 2
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

productAssociative :: Integer -> Integer -> Integer -> Bool
productAssociative x y z = x * (y * z) == (x * y) * z

productCommutative :: Integer -> Integer -> Bool
productCommutative x y = x * y == y * x

-- TODO: quickCheck will divide by zero every time, maybe we should use a custom Generator
prop_quotrem :: Integer -> Integer -> Bool
prop_quotrem x y = (quot x y) * y + (rem x y) == x

prop_divmod :: Integer -> Integer -> Bool
prop_divmod x y = (div x y)*y + (mod x y) == x

prop_half :: Double -> Bool
prop_half x = half x == half (halfIdentity x)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs = listOrdered (sort xs) == True

prop_reverse :: (Eq a) => [a] -> Bool
prop_reverse xs = reverse (reverse xs) == id xs

prop_moneysymbol :: (Eq b) => (a -> b) -> a -> Bool
prop_moneysymbol f x = (f $ x) == f x

prop_composition :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_composition f g x = ((f . g) x) == (f (g x))

--prop_lentake :: a -> [b] -> Bool
--prop_lentake n xs = length (take n xs) == n

runExerciseSpecs :: IO ()
runExerciseSpecs = do
  quickCheck prop_half
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck plusAssociative
  quickCheck plusCommutative
  quickCheck productAssociative
  quickCheck productCommutative
--  quickCheck prop_quotrem
--  quickCheck prop_divmod
  quickCheck (prop_reverse :: String -> Bool)
  quickCheck (prop_moneysymbol (+1) :: Int -> Bool )
  quickCheck (prop_composition (+1) (+2) :: Int -> Bool)
--  quickCheck (prop_lentake :: Integer -> String -> Bool)
