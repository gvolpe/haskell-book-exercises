import Data.Monoid
import Data.Foldable

-- for definition of xMinimum
newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n

-- for definition of xMaximum
newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y    = Max m
    | otherwise = Max n

-- functions
xSum :: (Foldable t, Num a) => t a -> a
xSum = getSum . foldMap Sum

xProduct :: (Foldable t, Num a) => t a -> a
xProduct = getProduct . foldMap Product

xElem :: (Foldable t, Eq a) => a -> t a -> Bool
xElem x xs = getAny $ foldMap (\e -> Any $ x == e) xs -- any . (==)

xMinimum :: (Foldable t, Ord a) => t a -> Maybe a
xMinimum xs = getMin $ foldMap (\e -> Min {getMin = Just e}) xs

xMaximum :: (Foldable t, Ord a) => t a -> Maybe a
xMaximum xs = getMax $ foldMap (\e -> Max {getMax = Just e}) xs

xNull :: (Foldable t) => t a -> Bool
xNull = foldr (\_ _ -> False) True

xLength :: (Foldable t) => t a -> Int
xLength = foldr (\x acc -> acc + 1) 0

xToList :: (Foldable t) => t a -> [a]
xToList = foldr (:) []

xFold :: (Foldable t, Monoid m) => t m -> m
xFold = foldr (\x acc -> x <> acc) mempty -- foldMap id

xFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
xFoldMap f = foldr (mappend . f) mempty
