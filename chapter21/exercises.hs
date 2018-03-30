{-# LANGUAGE UndecidableInstances #-}

import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance (Functor Identity, Foldable Identity) => Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance (Functor (Constant a), Foldable (Constant a)) => Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance (Functor Optional, Foldable Optional) => Traversable Optional where
  traverse _ Nada    = pure $ Nada
  traverse f (Yep x) = Yep <$> f x

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance (Functor List, Foldable List) => Traversable List where
  traverse _ Nil         = pure $ Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

--data Three a b c = Three a b c

--data Three' a b = Three' a b b

--data S n a = S (n a) a

--instance Traversable n => Traversable (S n) where
--traverse = undefined

--data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

--instance Functor Tree where
--fmap = undefined

--instance Foldable Tree where
--foldMap = undefined

--instance Traversable Tree where
--traverse = undefined
