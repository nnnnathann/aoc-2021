{-# LANGUAGE TupleSections #-}

module Submarine.LavaTubes where

import Control.Monad (filterM, join)
import Data.Char (digitToInt)
import Data.List (sort, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Windows
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
   where
    curr = values Map.! coord'

filterNeighbors :: ((a, a) -> Bool) -> Map Coord2 a -> Coord2 -> [Coord2]
filterNeighbors f values c =
  surrounding c
    |> mapMaybe (\c -> fmap (c,) (Map.lookup c values))
    |> filter (\(_, v) -> f (values Map.! c, v))
    |> map fst

lowCoords :: (Ord a) => [[a]] -> [Coord2]
lowCoords inp =
  neighboring inp
    |> filter isLowPoint
    |> map fst
 where
  values = coords inp
  isLowPoint (x, ns) =
    (values Map.! x) < minimum (mapMaybe (`Map.lookup` values) ns)

-- Grid operations

type Coord2 = (Int, Int)

neighbors :: a -> [[a]] -> [(a, [a])]
neighbors def inp =
  [ (v, neighbors)
  | (currCoord, currNeighbors) <- neighboring inp
  , let v = cs Map.! currCoord
  , let neighbors = map (fromMaybe def . flip Map.lookup cs) currNeighbors
  ]
 where
  cs = coords inp

neighboring :: [[a]] -> [(Coord2, [Coord2])]
neighboring inp =
  map (\c -> (c, surrounding c)) (Map.keys (coords inp))

surrounding :: Coord2 -> [Coord2]
surrounding (x, y) =
  [(x, y -1), (x, y + 1), (x -1, y), (x + 1, y)]

coords :: [[a]] -> Map Coord2 a
coords inp =
  Map.fromList
    [ ((x, y), xv)
    | (y, xs) <- zip [0 ..] inp
    , (x, xv) <- zip [0 ..] xs
    ]

inp :: [[Int]]
inp =
  [ [1, 2, 3]
  , [4, 1, 6]
  , [7, 8, 9]
  ]

riskLevel :: Int -> Int
riskLevel =
  (+) 1