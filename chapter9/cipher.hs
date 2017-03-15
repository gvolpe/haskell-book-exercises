module Cipher where

import Data.Char

type Shift = Int

encode :: Shift -> String -> [Int]
encode s = fmap (\x -> (ord x) + s)

decode :: Shift -> [Int] -> String
decode s= fmap (\x -> chr (x - s))
