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
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x    = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

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

main :: IO ()
main = do
  quickBatch $ applicative (ZipList' (Cons ("a", "b", "1") Nil))
