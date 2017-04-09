module ZipListMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- unfortunate orphan instances. Try to avoid these
-- in code you're going to keep or release.
-- this isn't going to work properly
instance Monoid a => Monoid (ZipList a) where
  mempty  = pure mempty -- ZipList []
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid (ZipList [1 :: Sum Int])
