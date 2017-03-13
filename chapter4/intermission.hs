data Mood = Blah | Woot deriving (Eq, Show)

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot
--changeMood x = if (x == Blah) then Woot else Blah
