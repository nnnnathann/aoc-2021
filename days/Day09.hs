module Main where

import AOC
import qualified Submarine.LavaTubes as LavaTubes

main :: IO ()
main = do
  init <- AOC.dayInputP 9 LavaTubes.parseHeightMap
  printE $ LavaTubes.sumRiskLowPoints <$> init
  printE $ LavaTubes.productTop3Basins <$> init