import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x
-- Try this out:
-- bind (\x -> [x+1]) [1,2,3]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n 
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c 

-- Ugly nested structures
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
   Nothing -> Nothing
   Just nammy ->
     case noNegative age' of
      Nothing -> Nothing
      Just agey ->
        case noNegative weight' of
         Nothing -> Nothing
         Just weighty ->
           weightCheck (Cow nammy agey weighty)

-- A bit better using do notation
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)
