{-# LANGUAGE TupleSections #-}

module Submarine.Hydrothermal (
  Line,
  parseMap,
  hotspotCount1,
  hotspotCount2,
) where

import Data.List (foldl')
import qualified Data.Map as Map
import Text.Megaparsec (parse, sepBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Extras (Parser, intP)
import Text.RawString.QQ (r)

type Line = ((Int, Int), (Int, Int))

-- Parsing

parseMap :: Parser [Line]
parseMap =
  sepBy lineP newline

lineP :: Parser Line
lineP = do
  a <- coordP
  _ <- string " -> "
  b <- coordP
  return (a, b)

coordP :: Parser (Int, Int)
coordP = do
  x <- intP
  _ <- char ','
  y <- intP
  return (x, y)

-- Calculations

hotspotCount1 :: [Line] -> Int
hotspotCount1 [] = 0
hotspotCount1 lines =
  Map.size $ Map.filter (1 <) $ pointGrid (filter vertOrHorz lines)

hotspotCount2 :: [Line] -> Int
hotspotCount2 [] = 0
hotspotCount2 lines =
  Map.size $ Map.filter (1 <) $ pointGrid lines

pointGrid :: [Line] -> Map.Map (Int, Int) Int
pointGrid = foldl' grid Map.empty
 where
  grid :: Map.Map (Int, Int) Int -> Line -> Map.Map (Int, Int) Int
  grid m line = Map.unionWith (+) m $ Map.fromList (map (,1) (points line))

points :: Line -> [(Int, Int)]
points line@((x1, y1), (x2, y2))
  | vertOrHorz line = [(x, y) | x <- range (x1, x2), y <- range (y1, y2)]
  | otherwise = [(x1 + d * increment x1 x2, y1 + d * increment y1 y2) | d <- [0 .. abs (x2 - x1)]]

range :: (Int, Int) -> [Int]
range (a, b) = [min a b .. max a b]

increment :: Int -> Int -> Int
increment a b = if a < b then 1 else -1

vertOrHorz :: Line -> Bool
vertOrHorz ((x1, y1), (x2, y2)) =
  x1 == x2 || y1 == y2
