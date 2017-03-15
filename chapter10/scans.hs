fibs      = 1 : scanl (+) 1 fibs
fibsNth n = fibs !! n

fibs20 = take 20 fibs

fibsLessThan100 = takeWhile (<100) fibs

fact :: Integer -> Integer
fact 0 = 1
fact n = foldl (*) n [1..(n-1)]
