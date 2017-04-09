module ApplicativeLaws where

-- 1) Identity: pure id <*> [1..5] == [1..5]
-- 2) Composition: pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3] == [(+1)] <*> ([(*2)] <*> [1, 2, 3])
-- 3) Homomorphism: pure (+1) <*> pure 1 == pure ((+1) 1)
-- 4) Interchange: Just (+3) <*> pure 1 == pure ($ 1) <*> Just (+3)

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty      = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

x = [("b", "w", 1)] :: [(String, String, Int)]  

main :: IO ()
main = do
  --quickBatch $ monoid Twoo -- Bad Monoid rules breaker
  quickBatch $ applicative x
