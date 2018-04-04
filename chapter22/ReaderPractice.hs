module ReaderPractice where

import Control.Applicative
import Data.Maybe
import Prelude hiding (lookup, uncurry)

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ []    = Nothing
lookup a (h:t) =
  if fst h == a
  then Just $ snd h
  else lookup a t

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = f <$> fst <*> snd
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is just uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = (+) <$> fst <*> snd

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

-- main
main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7
    print $ sequA $ fromMaybe 1 s'
    print $ bolt $ fromMaybe 1 ys
    print $ bolt $ fromMaybe 0 $ z' 1 -- exercise not clear
