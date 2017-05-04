{-# LANGUAGE FlexibleContexts #-}

-- Implement the Either Monad.
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x)  = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure                      = Second
  (First f)  <*> _          = First f
  _          <*> (First x)  = First x
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return     = pure
  (Second x) >>= f = f x
  (First x)  >>= _ = First x
