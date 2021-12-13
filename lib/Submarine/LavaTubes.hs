module Submarine.LavaTubes (
  parseHeightMap,
  sumRiskLowPoints,
  productTop3Basins,
  lowPoints,
  basins,
) where

import Data.Char (digitToInt)
import Data.Grid
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Flow ((|>))
import Text.Megaparsec (many)
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec.Extras (Parser, linesOf)

-- parse the int grid
parseHeightMap :: Parser [[Int]]
parseHeightMap =
  linesOf (many (digitToInt <$> digitChar))

-- part 1 : find the sum of the riskLevel of the lowest points
sumRiskLowPoints :: [[Int]] -> Int
sumRiskLowPoints =
  sum . map riskLevel . lowPoints

-- part 2 : starting from the lowest points, find segments
-- excluding 9s
productTop3Basins :: [[Int]] -> Int
productTop3Basins input =
  basins input
    |> map length
    |> sort
    |> reverse
    |> take 3
    |> product

lowPoints :: [[Int]] -> [Int]
lowPoints inp =
  lowCoords inp
    |> map (coords inp Map.!)

basins :: [[Int]] -> [[Int]]
basins inp =
  lowCoords inp
    |> map (segmentBy (uncurry isAscending) (coords inp))
    |> map Map.elems

isAscending :: Int -> Int -> Bool
isAscending x y =
  x < y && x /= 9 && y /= 9

segmentBy :: ((a, a) -> Bool) -> Map Coord2 a -> Coord2 -> Map Coord2 a
segmentBy f values coord
  | Map.size values == 0 = Map.empty
  | otherwise = go (Map.singleton coord (values Map.! coord)) coord
 where
  go prev coord' =
    filterNeighbors f values coord'
      |> map (segmentBy f values)
      |> Map.unions
      |> Map.union prev

lowCoords :: (Ord a) => [[a]] -> [Coord2]
lowCoords inp =
  neighboring inp
    |> filter isLowPoint
    |> map fst
 where
  values = coords inp
  isLowPoint (x, ns) =
    (values Map.! x) < minimum (mapMaybe (`Map.lookup` values) ns)

riskLevel :: Int -> Int
riskLevel =
  (+) 1