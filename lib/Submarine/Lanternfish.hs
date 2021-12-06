module Submarine.Lanternfish where

import Data.Map (Map)
import qualified Data.Map as Map
import Flow ((|>))
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Extras (Parser, intP)

-- Sweep is a report from the sub about the current lanternfish
parseSweep :: Parser [Int]
parseSweep = sepBy intP (char ',')

countAfterDaysN :: Int -> [Int] -> Int
countAfterDaysN n init = project initialMap n |> sum
 where
  initialMap = Map.fromListWith (+) [(i, 1) | i <- init]

project :: Map Int Int -> Int -> Map Int Int
project xs 0 = xs
project xs n = project next ((-) n 1)
 where
  next = Map.fromListWith (+) [b | a <- Map.toList xs, b <- step a]

step :: (Int, Int) -> [(Int, Int)]
step (t, count)
  | t == 0 = [(6, count), (8, count)]
  | otherwise = [(t -1, count)]