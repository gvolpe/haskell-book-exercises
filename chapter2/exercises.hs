module Exercises where

waxOn :: Num a => a -> a
waxOn x = x * 5
  where z = 7
        y = z + 8
        x = y * y -- same as y ^ 2

triple :: Num a => a -> a
triple x = x * 3

waxOff :: Num a => a -> a
waxOff x = triple x
