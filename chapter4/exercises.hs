awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

one :: [a] -> Int
one x = length x

four :: Int -> [a] -> Int
four x y = div x (length y)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Reading Syntax exercises

plusOne = (+ 1)

lenPlusOne x = plusOne w where w = length x

myId x = x

myHead (x:xs) = x

myFirst (x,y) = x
