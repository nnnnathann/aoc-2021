{-# LANGUAGE QuasiQuotes #-}

module Submarine.Diagnostic (
  Bit,
  parseReport,
  powerConsumption,
  lifeSupport,
) where

import Data.Foldable (Foldable (foldr'))
import Data.List
import Data.Maybe (catMaybes, fromMaybe)
import Text.Megaparsec (choice, many, oneOf, parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Extras (Parser, linesOf)
import Text.RawString.QQ

-- | A Bit is a single bit!
data Bit = Off | On deriving (Eq, Ord)

-- | How should we parse a bit? 0 = Off, 1 = On
instance Read Bit where
  readsPrec _ ('0' : xs) = [(Off, xs)]
  readsPrec _ ('1' : xs) = [(On, xs)]
  readsPrec _ _ = []

-- | Printing out a big should be 0 and 1.
instance Show Bit where
  show Off = "0"
  show On = "1"

-- | Report parser
parseReport :: Parser [[Bit]]
parseReport = linesOf diagnosticBinaryP

-- | Calculate the power consumption of a report (epsilon * gamma)
powerConsumption :: [[Bit]] -> Int
powerConsumption bs = product (map toInt [gammaRate bs, epsilonRate bs])

-- | Calculate the life support of a report (co2 scrubber * oxygen)
lifeSupport :: [[Bit]] -> Maybe Int
lifeSupport bs = do
  o2 <- oxygenRating bs
  co2 <- co2ScrubberRating bs
  return $ toInt co2 * toInt o2

-- | Get the integer rep of a bitstring (big endian)
toInt :: [Bit] -> Int
toInt = foldl' (\acc x -> acc * 2 + (case x of On -> 1; Off -> 0)) 0

-- | Parser for a bit string
diagnosticBinaryP :: Parser [Bit]
diagnosticBinaryP = many bitP

-- | Parser for a single bit
bitP :: Parser Bit
bitP = choice [On <$ char '1', Off <$ char '0']

-- | Complementary bit
complement :: Bit -> Bit
complement On = Off
complement Off = On

-- | Get columns of a list of [Bit] (rather than rows)
cols :: [[Bit]] -> [[Bit]]
cols = transpose

-- | Get the gamma rate of a report
gammaRate :: [[Bit]] -> [Bit]
gammaRate = map (mostCommonBit On) . cols

-- | Get the epsilon rate of a report
epsilonRate :: [[Bit]] -> [Bit]
epsilonRate = map (complement . mostCommonBit On) . cols

-- | Oxygen rating
oxygenRating :: [[Bit]] -> Maybe [Bit]
oxygenRating bs = findByBitMatch (mostCommonBit On) bs 0

-- | CO2 Scrubber rating
co2ScrubberRating :: [[Bit]] -> Maybe [Bit]
co2ScrubberRating bs = findByBitMatch (leastCommonBit Off) bs 0

{- | Find by matcher takes a selection function
     to select a single bit from a bitstring, and
     iterates over a list of bitstrings by position
     until only a single match is found
-}
findByBitMatch :: ([Bit] -> Bit) -> [[Bit]] -> Int -> Maybe [Bit]
findByBitMatch _ [] _ = Nothing
findByBitMatch _ [bs] _ = Just bs
findByBitMatch sel bs' i = findByBitMatch sel next (i + 1)
 where
  next :: [[Bit]]
  next = filter (f bs' i) bs'
  f :: [[Bit]] -> Int -> [Bit] -> Bool
  f bs'' i' b = (b !! i') == sel (cols bs'' !! i')

{- Specifications require the most common bit to be On in the
   case the two counts are equal
-}
mostCommonBit :: Bit -> [Bit] -> Bit
mostCommonBit ifEq bs = fromMaybe ifEq (mostCommon bs)

leastCommonBit :: Bit -> [Bit] -> Bit
leastCommonBit ifEq bs = maybe ifEq complement (mostCommon bs)

-- | Get the most common element of a list
mostCommon :: Ord a => [a] -> Maybe a
mostCommon = highest . counts
 where
  counts :: (Ord a) => [a] -> [(Int, a)]
  counts as' = [(length xs, head xs) | xs <- (group . sort) as']
  highest :: [(Int, a)] -> Maybe a
  highest xs
    | length ((group . sort) (map fst xs)) == 1 = Nothing
    | otherwise = Just ((snd . last . sortOn fst) xs)

testInput =
  parse
    parseReport
    "Test Input"
    [r|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|]
