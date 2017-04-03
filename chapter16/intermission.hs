import FunctorLaws
import Test.QuickCheck
import Test.QuickCheck.Function

-- Lifting exercises
a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi     = readIO "1" :: IO Integer
        changed = fmap ("123"++) $ fmap show ioi
    in fmap (*3) $ fmap (\x -> read x :: Integer) changed

-- Intermission exercises
newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two x) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Three x y) where
  fmap f (Three x y z) = Three x y (f z)

instance Functor (Three' x) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Functor (Four x y z) where
  fmap f (Four w x y z) = Four w x y (f z)

instance Functor (Four' x) where
  fmap f (Four' w x y z) = Four' w x y (f z)

-- Short exercise (Types similar to Maybe and Either respectively)
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

-- Verifying law abiding
type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do 
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose' :: IntFC)
