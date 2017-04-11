import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n Nil         = Nil
take' 1 (Cons x _)  = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x         = Cons x Nil
  _ <*> Nil      = Nil
  Nil <*> _      = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

-- ZipList Applicative
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> (List a)
repeat' x = xs
  where xs = Cons x xs

zipWith' :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipWith' _ Nil _                   = Nil
zipWith' _ _ Nil                   = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure x                          = ZipList' $ repeat' x
  _ <*> (ZipList' Nil)            = ZipList' Nil
  (ZipList' Nil) <*> _            = ZipList' Nil
  (ZipList' xs) <*> (ZipList' ys) = ZipList' (zipWith' id xs ys)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

-- Validation Applicative
data MySum a b = MyFirst a | MySecond b deriving (Eq, Show)

data MyValidation e a = MyError e | MySuccess a deriving (Eq, Show)

instance Functor (MySum a) where
  fmap _ (MyFirst x)  = MyFirst x
  fmap f (MySecond x) = MySecond (f x)

instance Applicative (MySum a) where
  pure                          = MySecond
  (MyFirst x) <*> _             = MyFirst x
  _ <*> (MyFirst y)             = MyFirst y
  (MySecond f) <*> (MySecond x) = MySecond (f x)

-- same as Sum/Either
instance Functor (MyValidation e) where
  fmap _ (MyError e)   = MyError e
  fmap f (MySuccess x) = MySuccess (f x) 

-- This is different
instance Monoid e => Applicative (MyValidation e) where
  pure                            = MySuccess
  (MyError e) <*> (MyError e')    = MyError (e <> e')
  (MyError e) <*> _               = MyError e
  _ <*> (MyError e)               = MyError e
  (MySuccess f) <*> (MySuccess x) = MySuccess (f x)

instance (Eq a, Eq b) => EqProp (MySum a b) where (=-=) = eq

instance (Eq a, Eq b) => EqProp (MyValidation a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (MySum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [MyFirst a, MySecond b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyValidation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [MyError a, MySuccess b]

-- More exercises: Write Applicative instances
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure                          = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- not going to write Arbitrary and Eq instances for the rest... boring...
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x                    = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x                   = Two mempty x
  (Two x f) <*> (Two x' y) = Two (x <> x') (f y)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x                            = Three mempty mempty x
  (Three x y f) <*> (Three x' y' z) = Three (x <> x') (y <> y') (f z)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x                              = Three' mempty x x
  (Three' x f f') <*> (Three' x' y z) = Three' (x <> x') (f y) (f z)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x w y z) = Four x w y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x                               = Four mempty mempty mempty x
  (Four x w y f) <*> (Four x' w' y' z) = Four (x <> x') (w <> w') (y <> y') (f z)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x w y z) = Four' x w y (f z)

instance Monoid a => Applicative (Four' a) where
  pure x                                 = Four' mempty mempty mempty x 
  (Four' x w y f) <*> (Four' x' w' y' z) = Four' (x <> x') (w <> w') (y <> y') (f z) 

-- Tests
main :: IO ()
main = do
  --quickBatch $ applicative (Identity ("a", "b", 1 :: Int))
  quickBatch $ applicative (ZipList' (Cons ("a", "b", 1 :: Int) Nil))
  --quickBatch $ applicative (MySecond ("a", "b", 1 :: Int))
  --quickBatch $ applicative (MySuccess ("a", "b", 1 :: Int))
