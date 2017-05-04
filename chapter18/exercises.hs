import Control.Applicative
import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Doesn't make sense (?)
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg 

instance Applicative Nope where 
  pure x  = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return  = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

-- Flipped Either
data PhhhbbtttEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (PLeft x)  = PLeft (f x)
  fmap f (PRight x) = PRight x

instance Applicative (PhhhbbtttEither b) where
  pure                      = PLeft
  (PRight f) <*> _          = PRight f
  _          <*> (PRight x) = PRight x
  (PLeft f)  <*> (PLeft x)  = PLeft (f x)

instance Monad (PhhhbbtttEither b) where
  return           = pure
  (PLeft x)  >>= f = f x
  (PRight x) >>= _ = PRight x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [PLeft b, PRight a]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure                          = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return             = pure
  (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n Nil         = Nil
take' 1 (Cons x _)  = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x         = Cons x Nil
  _ <*> Nil      = Nil
  Nil <*> _      = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

instance Monad List where
  return            = pure
  Nil >>= _         = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

-- More functions
j :: Monad m => m (m a) -> m a -- join
j m = m >>= id 

l1 :: Monad m => (a -> b) -> m a -> m b -- liftM
l1 f m = m >>= (\x -> return $ f x)

-- There probably is a better way to implement this using composition...
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c -- liftM2
l2 f m n = (m >>= (\x -> return $ f x)) >>= (\g -> n >>= (\y -> return $ g y))

a :: Monad m => m a -> m (a -> b) -> m b -- flip ap
a m n = m >>= (\x -> n >>= (\f -> return $ f x))

-- Another way is by using recursion
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs fm = sequence $ xs >>= (\x -> return $ fm x) 

flipType :: (Monad m) => [m a] -> m [a] -- sequence: [Just 1, Just 2] would be Just [1, 2] 
flipType xs = meh xs $ join . pure

-- All the tests!
main = do
  let t1 = undefined :: Nope (Int, String, Int)
  let t2 = undefined :: PhhhbbtttEither String (Int, String, Int)
  let t3 = undefined :: Identity (Int, String, Int)
  let t4 = undefined :: List (Int, String, Int)
  quickBatch $ functor t1
  quickBatch $ applicative t1
  quickBatch $ monad t1
  quickBatch $ functor t2
  quickBatch $ applicative t2
  quickBatch $ monad t2
  quickBatch $ functor t3
  quickBatch $ applicative t3
  quickBatch $ monad t3
  quickBatch $ functor t4
  quickBatch $ applicative t4
  quickBatch $ monad t4

