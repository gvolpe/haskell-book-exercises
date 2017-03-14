sumsAll :: (Eq a, Num a) => a -> a
sumsAll 0 = 0
sumsAll n = n + sumsAll (n - 1)

multiplyR :: (Integral a) => a -> a -> a
multiplyR x 0 = 0
multiplyR x y = x + multiplyR x (y - 1)

data DividedResult = Result Integer | DividedByZero deriving Show

--dividedBy :: Integral a => a -> a -> DividedResult
dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = Result value
  where value 
         | num < 0 && denom < 0 = result
         | num < 0 || denom < 0 = negate result
         | otherwise            = result
        result = simpleDiv num denom

simpleDiv :: Integral a => a -> a -> a
simpleDiv num denom = loop (abs num) (abs denom) 0
 where loop n d acc
        | n < d = acc
        | otherwise = loop (n - d) d (acc + 1)

mc91 :: Integral a => a -> a
mc91 x
  | x > 100   = x - 10
  | otherwise = 91
