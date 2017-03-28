import Data.Monoid
import MonoidLaws
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada
  mappend Nada Nada         = Nada
  mappend (Only x) (Only y) = Only (x <> y) 
  mappend (Only x) Nada     = Only x
  mappend Nada     (Only y) = Only y

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty  = First' Nada
  mappend (First' Nada) (First' y) = First' y
  mappend (First' (Only x))     _  = First' (Only x)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

--exercise :: IO ()
--exercise = do
--  quickCheck (monoidAssoc :: FirstMappend)
--  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  --quickCheck (monoidRightIdentity :: First' String -> Bool)
