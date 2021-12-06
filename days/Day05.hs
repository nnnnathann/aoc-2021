module Main where

import AOC
import Submarine.Hydrothermal as Hydrothermal

main :: IO ()
main = do
  day5 <- AOC.dayInputP 5 Hydrothermal.parseMap
  putStrLn "Submarine Hydrothermal Report"
  putStrLn "==========================="
  putStrLn "Number of Hotspots (horz / vert):"
  AOC.printE $ Hydrothermal.hotspotCount1 <$> day5
  putStrLn "Number of Hotspots (horz / vert / diag):"
  AOC.printE $ Hydrothermal.hotspotCount2 <$> day5