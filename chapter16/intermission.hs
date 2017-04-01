a = fmap (+1) $ read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi     = readIO "1" :: IO Integer
        changed = fmap ("123"++) $ fmap show ioi
    in fmap (*3) $ fmap (\x -> read x :: Integer) changed
