module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTripAlt :: (Show a, Read b) => a -> b
roundTripAlt = read . show

main = do
  print (roundTripAlt 4 :: Integer) -- roundTrip 4
  print (id 4)
