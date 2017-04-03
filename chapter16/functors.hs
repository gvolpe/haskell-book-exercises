data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)

data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x) 

-- ``lifted'' because they've been lifted over
-- some structure f
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- more functors
data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
