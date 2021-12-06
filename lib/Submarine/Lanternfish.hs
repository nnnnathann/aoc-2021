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

-- Predict the number of lanternfish that will be around
-- after N days
countAfterDaysN :: Int -> [Int] -> Int
countAfterDaysN n init = project initialMap n |> sum
 where
  initialMap = Map.fromListWith (+) [(i, 1) | i <- init]

-- Project accepts an initial state, and how many days
-- out to project the lanternfish state, and iterates
-- the state forward day by day
project :: Map Int Int -> Int -> Map Int Int
project xs 0 = xs
project xs n = project next ((-) n 1)
 where
  next = Map.fromListWith (+) [b | a <- Map.toList xs, b <- step a]

-- Given a single timer + count value, iterate
-- to the next timer + count value / values
-- (in the case more than one are produced they should
-- be summed (see above))
step :: (Int, Int) -> [(Int, Int)]
step (t, count)
  | t == 0 = [(6, count), (8, count)]
  | otherwise = [(t -1, count)]