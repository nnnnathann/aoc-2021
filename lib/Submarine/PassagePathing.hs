module Submarine.PassagePathing where

import Control.Arrow ((>>>))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Flow ((|>))
import Text.Megaparsec (choice, many, oneOf, some)
import Text.Megaparsec.Char (char, lowerChar, string, upperChar)
import Text.Megaparsec.Extras (Parser, linesOf)

data Strategy
  = AtMostOnce
  | SingleSmallCaveTwice

data Cave
  = Start
  | End
  | Small String
  | Large String
  deriving (Ord, Show, Eq)

data Passage = Passage Cave Cave
  deriving (Show, Eq)

parseCaves :: Parser [Passage]
parseCaves =
  concatMap dupe <$> linesOf passage
 where
  dupe p@(Passage Start c2) = [p]
  dupe p@(Passage _ End) = [p]
  dupe p@(Passage c1 c2) = [p, Passage c2 c1]
  passage = do
    start <- cave
    char '-'
    Passage start <$> cave
  cave =
    choice
      [ Start <$ string "start"
      , End <$ string "end"
      , Small <$> some lowerChar
      , Large <$> some upperChar
      ]

type Caves = [Cave]

countPaths :: Strategy -> [Passage] -> Int
countPaths s ps = length $ enumerate s ps

enumerate :: Strategy -> [Passage] -> [[Cave]]
enumerate AtMostOnce passages = dedupe $ walk (allCaves passages) passages []
enumerate SingleSmallCaveTwice passages =
  dedupe $
    [ a
    | c <- dedupe $ filterCaves isSmall passages
    , a <- walk (c : allCaves passages) passages []
    ]

allCaves :: [Passage] -> [Cave]
allCaves = dedupe . filterCaves (const True)

dedupe :: Ord a => [a] -> [a]
dedupe = Set.toList . Set.fromList

walk :: [Cave] -> [Passage] -> [Cave] -> [[Cave]]
walk allowed passages prev@(End : xs) = [reverse prev]
walk allowed passages prev@(s : xs) =
  [ sub
  | p@(Passage s' e) <- passages
  , s' == s
  , isJust $ find (== e) allowed
  , sub <- walk (visitCave e allowed) passages (e : prev)
  ]
walk allowed passages [] = walk allowed passages [Start]

-- | Allow small caves to be used more than once
visitCave :: Cave -> [Cave] -> [Cave]
visitCave c@(Small _) = removeFirst c
visitCave Start = filter (Start ==)
visitCave End = filter (End ==)
visitCave _ = id

-- | Removes the first occurance of an element in a list
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x (x' : xs)
  | x == x' = xs
  | otherwise = x' : removeFirst x xs
removeFirst _ [] = []

filterCaves :: (Cave -> Bool) -> [Passage] -> [Cave]
filterCaves f passages =
  [a | Passage a _ <- passages, f a]
  ++ [b | Passage _ b <- passages, f b]

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False