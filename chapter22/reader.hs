{-# Language InstanceSigs #-}

import Control.Applicative (liftA2)
import Text.Show.Functions

newtype Reader r a = Reader { runReader :: r -> a } deriving (Show)

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- exercise 1
ask :: Reader a a
ask = Reader $ id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName   = DogName String deriving (Eq, Show)
newtype Address   = Address String deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- with Reader, alternate
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- exercise 2
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- exercise 3
asks :: (r -> a) -> Reader r a
asks f = Reader $ \x -> f x

-- exercise 4
getDogR'' :: Reader Person Dog
getDogR'' = Reader $ \p -> Dog (dogName p) (address p)

-- exercise Monadic impl
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader $ \p -> dogName p
  addy <- Reader $ \p -> address p
  return $ Dog (name) (addy)
