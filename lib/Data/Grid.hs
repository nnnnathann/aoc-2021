{-# LANGUAGE TupleSections #-}

module Data.Grid where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Flow ((|>))

-- Grid operations

type Coord2 = (Int, Int)

filterNeighbors :: ((a, a) -> Bool) -> Map Coord2 a -> Coord2 -> [Coord2]
filterNeighbors f values c =
  surrounding c
    |> mapMaybe (\c -> fmap (c,) (Map.lookup c values))
    |> filter (\(_, v) -> f (values Map.! c, v))
    |> map fst

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

touching :: Coord2 -> [Coord2]
touching c =
  [ (x + dx, y + dy)
  | let (x, y) = c
  , dx <- [-1 .. 1]
  , dy <- [-1 .. 1]
  ]

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

uncoords :: Map Coord2 a -> [[a]]
uncoords cs =
  [ xs
  | let ks = Map.keys cs
  , y <- [0 .. maximum (map snd ks)]
  , let xs = map (\x -> cs Map.! (x, y)) [0 .. maximum (map fst ks)]
  ]