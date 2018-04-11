{-# Language InstanceSigs #-}

import Text.Show.Functions

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) } deriving (Show)

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \h ->
    let a = fst $ g h
    in (f a, h)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \h ->
    let ab = fst $ f h
        a  = fst $ g h
    in (ab a, h)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \h ->
    let a  = fst $ f h
        b  = fst $ runMoi (g a) h
    in (b, h)
