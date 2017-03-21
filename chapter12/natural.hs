module Naturals where

data Nat = Zero | Succ Nat deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + (natToInteger x)

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
validIntToNat :: Integer -> Nat
validIntToNat 0 = Zero
validIntToNat x = Succ $ validIntToNat (x - 1)

integerToNat :: Integer -> Maybe Nat
integerToNat x = case x >= 0 of
  True  -> Just (validIntToNat x)
  False -> Nothing
