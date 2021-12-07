module Submarine.WhaleDefense where

import Data.Foldable (minimumBy)
import qualified Data.Map as Map
import Flow ((|>))
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Extras (Parser, intP)

parsePositions :: Parser [Int]
parsePositions = sepBy intP (char ',')

totalFuel1 :: [Int] -> Int
totalFuel1 = totalFuel totalDistanceTo1

totalFuel2 :: [Int] -> Int
totalFuel2 = totalFuel totalDistanceTo2

totalFuel :: ([Int] -> Int -> Int) -> [Int] -> Int
totalFuel f xs = snd . minPair $ Map.fromList $ [(x, f xs x) | x <- [0 .. maximum xs]]

totalDistanceTo1 :: [Int] -> Int -> Int
totalDistanceTo1 = totalDistanceWith dist

-- Total distance with incrementing fuel cost
totalDistanceTo2 :: [Int] -> Int -> Int
totalDistanceTo2 = totalDistanceWith (\x x' -> triangularNum (dist x x'))

-- Distance is the distance between two points
dist :: Int -> Int -> Int
dist x y = abs (x - y)

-- Triangular number is the sum of the first n natural numbers
triangularNum :: Integral a => a -> a
triangularNum n = (n * n + n) `div` 2

totalDistanceWith :: (Int -> Int -> Int) -> [Int] -> Int -> Int
totalDistanceWith f xs x =
  map (f x) xs |> sum

minPair :: (Ord a) => Map.Map k a -> (k, a)
minPair m =
  Map.toList m
    |> minimumBy (\(_, a) (_, a') -> compare a a')
