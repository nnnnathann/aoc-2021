module Submarine.LavaTubes where

import Control.Monad (join)
import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Windows
import Text.Megaparsec (many)
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec.Extras (Parser, linesOf)

parseHeightMap :: Parser [[Int]]
parseHeightMap =
  linesOf (many (digitToInt <$> digitChar))

sumRiskLowPoints :: [[Int]] -> Int
sumRiskLowPoints =
  sum . map riskLevel . lowPoints

lowPoints :: [[Int]] -> [Int]
lowPoints inp =
  [ v
  | (v, ns) <- neighbors 9 inp
  , v < minimum ns
  ]

type Coord2 = (Int, Int)

neighbors :: a -> [[a]] -> [(a, [a])]
neighbors def inp =
  [ (v, neighbors)
  | (currCoord, currNeighbors) <- ns
  , let v = cs Map.! currCoord
  , let neighbors = map (fromMaybe def . flip Map.lookup cs) currNeighbors
  ]
 where
  cs = coords inp
  ns :: [(Coord2, [Coord2])]
  ns = map (\c -> (c, surrounding c)) (Map.keys cs)

surrounding :: Coord2 -> [Coord2]
surrounding (x, y) =
  [(x, y -1), (x, y + 1), (x -1, y), (x + 1, y)]

coords :: [[a]] -> Map (Int, Int) a
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