module Combinations where

import Control.Applicative (liftA3)

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos a b c = liftA3 (,,) a b c
