{-# LANGUAGE FlexibleInstances #-}

-- Fic Functor instances
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

data Company a b c = DeepBlue a c | Something b deriving (Eq, Show)

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a
  fmap f (R b a b') = R (f b) a (f b)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip More x) where
  fmap f (Flip (L a b a')) = Flip $ L (f a) b (f a')
  fmap f (Flip (R b a b')) = Flip $ R  b (f a) b'

-- Write Functor instances
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant x) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor x) = Bloor (f x)

newtype K a b = K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst x) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut $ fmap g x

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa x y) = DaWrappa (fmap h x) (fmap h y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething x y) = IgnoringSomething x $ fmap h y

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

data MyList a = Nil | Cons a (MyList a) deriving (Eq, Show)

instance Functor MyList where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g)    = Read $ fmap f g
