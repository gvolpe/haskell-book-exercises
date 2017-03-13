module Chapter3 where

oneA :: [Char] -> [Char]
oneA x = x ++ "!"

oneB :: String -> String
oneB x = [x !! 4] --[head (drop 4 x)]

oneC :: String -> String
oneC x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 3

currying = "Curry is awesome"

letterIndex :: Int -> Char
letterIndex x = currying !! x

rvrs :: String
rvrs = (drop 9 currying) ++ take 4 (drop 5 currying) ++ (take 5 currying)
