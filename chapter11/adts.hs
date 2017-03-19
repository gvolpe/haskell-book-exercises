{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- newtype keyword
-- it has no runtime overhead, as it reuses the representation of the type it contains
-- type == newtype for the compiler but it helps readability for human beings :)
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows  = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Horses = Horses Int deriving (Eq, Show, TooMany)

-- Don't needed if we derive it using the generalized pragma at the top
--instance TooMany Goats where
--  tooMany (Goats n) = tooMany n

-- intermission exercises
instance TooMany (Int, String) where
  tooMany (x, y) = tooMany x

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

--instance TooMany (Num a, TooMany a) where
--  tooMany (x, y) = tooMany (x + y)

data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b }deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig
