-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe x = case x == "the" of
  True  -> Nothing
  False -> Just x

maybeToString :: Maybe String -> String
maybeToString (Just x)  = x ++ " "
maybeToString Nothing   = "a "

dropRight :: Int -> [a] -> [a]
dropRight x = reverse . drop x . reverse

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe x = dropRight 1 $ concat $ map maybeToString f
  where f = map notThe $ words x

wordAfterThe :: [String] -> String
wordAfterThe (x:xs) = case x == "the" of
  True  -> head xs
  False -> wordAfterThe xs

vowels = "aeiou"

startsWithVowel :: String -> Bool
startsWithVowel []    = False
startsWithVowel (x:_) = elem x vowels

countTheBeforeVowel :: String -> Bool
countTheBeforeVowel = startsWithVowel . wordAfterThe . words

countVowels :: String -> Integer
countVowels ""     = 0
countVowels (x:xs) = if elem x vowels then 1 + (countVowels xs) else countVowels xs

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord x = if condition then Nothing else Just (Word' x)
  where condition = vowels > (fromIntegral $ length x) - vowels
        vowels = countVowels x

-- iterate, unfoldr

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

as' :: Maybe (a, b) -> [a]
as' (Just (x, _)) = [x]
as' Nothing       = []

bs' :: Maybe (a, b) -> [b]
bs' (Just (_, y)) = [y]
bs' Nothing       = []

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = (as' $ f x) ++ myUnfoldr f (head $ bs' $ f x)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x
