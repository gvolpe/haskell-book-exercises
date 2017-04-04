import Control.Applicative
import Data.List (elemIndex)

-- Short exercises
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

w :: Maybe Integer
w = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> w <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x y

xs = [1, 2, 3]
ys = [4, 5, 6]

x1 :: Maybe Integer
x1 = lookup 3 $ zip xs ys

y1 :: Maybe Integer
y1 = lookup 2 $ zip xs ys

-- I don't get this one...
summed :: Maybe Integer
summed = fst $ sum <$> (,) x1 y1

-- Write an Applicative instance for Identity.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x) 

instance Applicative Identity where
  pure                      = Identity 
  Identity f <*> Identity x = Identity (f x)

--Write an Applicative instance for Constant.
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _                    = Constant mempty
  Constant f <*> Constant x = Constant $ mappend f x

-- More exercises
a = const <$> Just "Hello" <*> pure "World"
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3] 
