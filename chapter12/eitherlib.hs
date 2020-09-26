module EitherSmallLib where

getLeft :: Either a b -> [a]
getLeft (Left x) = [x]
getLeft _        = []

getRight :: Either a b -> [b]
getRight (Right x) = [x]
getRight _         = []

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> (getLeft x) ++ acc) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> (getRight x) ++ acc) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Right x) = g x
either' f g (Left x)  = f x

-- (a -> c) can just return Nothing
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just $ f x)
