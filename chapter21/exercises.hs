{-# LANGUAGE UndecidableInstances #-} -- This is needed for all the constrains (Functor, Foldable). The compiler will tell you need FlexibleContexts instead.

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

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance (Functor (Three a b), Foldable (Three a b)) => Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

data Three' a b = Three' a b b deriving (Eq, Ord, Show)

instance (Functor (Three' a), Foldable (Three' a)) => Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c

data S n a = S (n a) a deriving (Eq, Ord, Show)

instance (Functor (S n), Foldable (S n), Traversable n) => Traversable (S n) where
  traverse f (S x y) = S <$> traverse f x <*> f y

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = (foldMap f l) <> (f x) <> (foldMap f r)

instance Traversable Tree where
  traverse _ Empty        = pure $ Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

