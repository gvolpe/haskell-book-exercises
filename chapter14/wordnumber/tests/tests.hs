module WordNumberTest where

import Data.Char (toUpper)
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = (toUpper x) : xs

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

capitalizeSpec :: IO ()
capitalizeSpec = hspec $ do
  describe "Capitalize Word does what we expect" $ do
    it "returns an empty string for an empty string" $ do
      capitalize "" `shouldBe` ""
    it "capitalize the only letter for a string of one letter" $ do
      capitalize "h" `shouldBe` "H"
    it "returns Gabi for gabi" $ do
      capitalize "gabi" `shouldBe` "Gabi"

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

idempotenceTwice :: String -> Bool
idempotenceTwice x = (capitalize x) == (twice capitalize) x

idempotenceFourTimes :: String -> Bool
idempotenceFourTimes w = (capitalize w) == (fourTimes capitalize) w

idempotenceSort :: (Ord a, Eq a) => [a] -> Bool
idempotenceSort x = (sort x) == (twice sort) x && (sort x) == (fourTimes sort) x

idempotenceSpec :: IO ()
idempotenceSpec = do
  quickCheck idempotenceTwice
  quickCheck idempotenceFourTimes
  quickCheck (idempotenceSort :: String -> Bool)

-- Make your own Generators for the following type:

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFool' :: Gen Fool
genFool' = frequency [ (2, return Fulse)
                     , (1, return Frue) ]
