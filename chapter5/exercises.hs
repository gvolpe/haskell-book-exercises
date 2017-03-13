-- Write a type signature
functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

-- Yes, c & c'' are the same
c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

-- r could be named tail
r :: [a] -> [a]
r (x:xs) = xs
r     [] = []

-- composition
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' = f x
