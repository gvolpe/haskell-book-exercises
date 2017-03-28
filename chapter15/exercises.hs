import Data.Monoid (Sum, Monoid)
import Data.Semigroup
import MonoidLaws (monoidLeftIdentity, monoidRightIdentity)
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty  = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Identity 
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y) 

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty  = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two w z) = Two (x <> w) (y <> z)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

-- Three
data Three a b c = Three a b c

-- Four
data Four a b c d = Four a b c d

-- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _               <> _               = BoolConj False

instance Monoid BoolConj where
  mempty  = BoolConj True
  mappend = (<>)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    elements [(BoolConj a), (BoolConj a)]

-- BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _               = BoolDisj False

instance Monoid BoolDisj where
  mempty  = BoolDisj False
  mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    elements [(BoolDisj a), (BoolDisj a)]

-- Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst x) <> (Fst y) = Fst y
  (Fst x) <> (Snd y) = Snd y
  (Snd x) <> _       = Snd x
 
type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary 
    elements [(Fst a), (Snd b)]

-- Combine (I don't get the point)
newtype Combine a b = Combine { unCombine :: (a -> b) }

-- Validation
data MyValidation a b = MyFailure a | MySuccess b deriving (Eq, Show)

instance Semigroup a => Semigroup (MyValidation a b) where
  (MySuccess x) <> (MySuccess y) = MySuccess y
  (MyFailure x) <> (MyFailure y) = MyFailure (x <> y)
  _             <> (MyFailure y) = MyFailure y
  (MyFailure x) <> _             = MyFailure x

type MyValidationAssoc = MyValidation String Int -> MyValidation String Int -> MyValidation String Int -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyValidation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(MyFailure a), (MySuccess b)]

-- Validation AccumulateRight
newtype AccumulateRight a b = AccumulateRight (MyValidation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
 (AccumulateRight (MySuccess x)) <> (AccumulateRight (MySuccess y)) = AccumulateRight (MySuccess (x <> y))
 (AccumulateRight (MySuccess x)) <> (AccumulateRight (MyFailure y)) = AccumulateRight (MyFailure y)
 (AccumulateRight (MyFailure x)) <> _                               = AccumulateRight (MyFailure x)

type AccumulateRightAssoc = AccumulateRight String Trivial -> AccumulateRight String Trivial -> AccumulateRight String Trivial -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(AccumulateRight (MyFailure a)), (AccumulateRight (MySuccess b))]

-- Validation AccumulateBoth
newtype AccumulateBoth a b = AccumulateBoth (MyValidation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
 (AccumulateBoth (MySuccess x)) <> (AccumulateBoth (MySuccess y)) = AccumulateBoth (MySuccess (x <> y))
 (AccumulateBoth (MySuccess x)) <> (AccumulateBoth (MyFailure y)) = AccumulateBoth (MyFailure y)
 (AccumulateBoth (MyFailure x)) <> (AccumulateBoth (MySuccess y)) = AccumulateBoth (MyFailure x)
 (AccumulateBoth (MyFailure x)) <> (AccumulateBoth (MyFailure y)) = AccumulateBoth (MyFailure (x <> y))

type AccumulateBothAssoc = AccumulateBoth String Trivial -> AccumulateBoth String Trivial -> AccumulateBoth String Trivial -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(AccumulateBoth (MyFailure a)), (AccumulateBoth (MySuccess b))]

-- Tests
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: MyValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Two String Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Two String Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

-- Mem test: Looks like the State Monad but I don't get it...
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty  = Mem (\s -> (mempty, s))
  mappend = undefined

f' = Mem $ \s -> ("hi", s + 1)

--memtest = do
--  print $ runMem (f' <> mempty) 0
--  print $ runMem (mempty <> f') 0
--  print $ (runMem mempty 0 :: (String, Int))
--  print $ runMem (f' <> mempty) 0 == runMem f' 0
--  print $ runMem (mempty <> f') 0 == runMem f' 0
