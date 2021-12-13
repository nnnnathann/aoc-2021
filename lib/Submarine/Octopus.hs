{-# LANGUAGE TupleSections #-}

module Submarine.Octopus where

import Data.Grid
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Flow
import Text.Megaparsec (many)
import Text.Megaparsec.Extras (Parser, digitLines)

parseGrid :: Parser [[Int]]
parseGrid = digitLines

-- Count the number of flashes in 100 steps
solvePart1 :: [[Int]] -> Int
solvePart1 input =
  flashCount input
    |> take 100
    |> sum

solvePart2 :: [[Int]] -> Int
solvePart2 input =
  elemIndex (length input ^ 2) (flashCount input)
    |> fmap (+ 1)
    |> fromMaybe 0

flashCount :: [[Int]] -> [Int]
flashCount input = simulate input |> fmap (Map.size . Map.filter (== 0))

-- iterator for steps in the octopus simulation
simulate :: [[Int]] -> [Map Coord2 Int]
simulate inp = coords inp |> iterate stepAll |> tail

-- performs a single step of the simulation
stepAll :: Map Coord2 Int -> Map Coord2 Int
stepAll m =
  foldl
    flash
    -- start by adding one to each energy level
    (fmap (1 +) m)
    -- fold over the updates for the ones ready to flash
    [flashCoord | (flashCoord, 9) <- Map.toList m]

-- octopus flash!
flash :: Map Coord2 Int -> Coord2 -> Map Coord2 Int
flash m c = case Map.lookup c m of
  Just energy
    | energy > 8 -> foldl flash (Map.insert c 0 m) (touching c)
    | energy > 0 -> Map.insert c (energy + 1) m
    | otherwise -> m
  _ -> m