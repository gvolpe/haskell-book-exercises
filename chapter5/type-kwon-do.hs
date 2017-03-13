module TypeKwonDo where

data Woot
data Blah

f :: Woot -> Blah
f x = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (x, y) = (x, x)

-- one
f1 :: Int -> String
f1 x = show x

g1 :: String -> Char
g1 (x:xs) = x

h1 :: Int -> Char
h1 x = head $ show x

-- two
data A
data B
data C

q :: A -> B
q x = undefined

w :: B -> C
w x = undefined

e :: A -> C
e x = w $ q x

-- three
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- four
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x
