import Data.Char
import Data.Function (on)
import Data.List (elemIndex, groupBy, sortBy)
import Data.Ord (comparing)
import Data.Tuple (swap)

type Digit = Char
type PhoneValues = String

data DaPhone = DaPhone [(Digit, PhoneValues)] deriving (Eq, Show)

daphone = DaPhone 
  [('1', "1"),
   ('2', "abc"),
   ('3', "def"),
   ('4', "ghi"),
   ('5', "jkl"),
   ('6', "mno"),
   ('7', "pqrs"),
   ('8', "tuv"),
   ('9', "wxyz"),
   ('*', "*^"),
   ('0', " +_"),
   ('#', "#.,")]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

-- Valid presses: 1 and up
type Presses = Int

indexOf :: Eq a =>  a -> [a] -> Int
indexOf x xs = case index of
  Just v  -> v + 1
  Nothing -> 0
  where index = elemIndex x xs 

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone x) v = foldr f [] y
  where y = map swap x
        g = (\(c,d) acc -> if isUpper v && elem (toLower v) c then ('*', 1) : (d, indexOf (toLower v) c) : acc else acc)
        f = (\(c,d) acc -> if elem v c then (d, indexOf v c) : acc else g (c,d) acc)
-- assuming the default phone definition
-- 'a' -> ('2', 1)
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p x = concat $ map (reverseTaps p) x

groupByIdentity :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
groupByIdentity  = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map (\(a,b) -> sum b) . groupByIdentity

--mostPopularLetter :: String -> Char

mostPressedDigit :: [(Digit, Presses)] -> (Digit, Presses)
mostPressedDigit = swap . maximum . map swap . map (\(a,b) -> (a, sum b)) . groupByIdentity
