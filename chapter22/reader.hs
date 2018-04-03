import Control.Applicative

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

m' :: Integer -> Integer
m' = fmap hurr durr

-- parallel computation using the Applicative
m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

-- sequential (monadic) computation
hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)
