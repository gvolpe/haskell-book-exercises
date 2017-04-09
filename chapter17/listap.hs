import Data.Monoid

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys 

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

instance Applicative List where
  pure x         = Cons x Nil
  Nil <*> _      = Nil
  _ <*> Nil      = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs
