import Data.Monoid

data Constant a b = Constant a deriving Show

instance Foldable (Constant b) where
  foldMap _ _ = mempty

data Two a b = Two a b deriving Show

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if f x then mempty else pure x)
