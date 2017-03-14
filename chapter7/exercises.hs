tensDigit :: Integral a => a -> a
tensDigit x = snd (divMod x 10)
--  where xLast = div x 10
--        d     = mod xLast 10

hunsDigit :: Integral a => a -> a
hunsDigit x = g x
  where f = flip divMod
        g = snd . f 100

foldBool :: a -> a -> Bool -> a
foldBool = error "Error: Need to implement foldBool!"

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
 | z == True  = x
 | z == False = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True  = x
foldBool3 x y False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y) 
