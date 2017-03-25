module VigenereCipher (encode, decode) where

import Data.Char

type Shift = Int

keyword = "ALLY"

magicZip :: String -> String -> [(Char, Char)]
magicZip [] _ = []
magicZip _ [] = []
magicZip (x:xs) (y:ys)
  | x == ' '  = (x,x) : magicZip xs (y : ys)
  | otherwise = (x,y) : magicZip xs ys

shift :: Char -> Shift
shift x
  | x == head keyword || x == ' ' =  0
  | otherwise                     = ord x - ord (head keyword)

shiftings :: (Char -> Shift) -> String -> [Shift]
shiftings f x = map (f . snd) $ magicZip x keywordStream
  where keywordStream = concat $ repeat keyword

codec :: ((Char, Shift) -> Char) -> String -> String
codec f x = map f valueWithShifts
  where valueWithShifts = zip x $ shiftings shift x

encode :: String -> String
encode = codec (\(x,y) -> chr $ (ord x) + y)

decode :: String -> String
decode = codec (\(x,y) -> chr $ (ord x) - y)
