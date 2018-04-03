import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

-- Two simple functions with the same type, taking the same type of input. We could compose them, using (.) or fmap :
composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- We will want to use an applicative here. The type will look like this:
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

-- Monadic implementation
tupledM :: [Char] -> ([Char], [Char])
tupledM = do
   a <- cap
   b <- rev
   return (a, b)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= \a -> rev >>= \b -> return (a, b)
