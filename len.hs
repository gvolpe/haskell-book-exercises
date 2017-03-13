len :: [a] -> Int
len (x:xs) = 1 + len(xs)
len     [] = 0
