module Main where

import AOC
import qualified Submarine.WhaleDefense as WhaleDefense

main :: IO ()
main = do
  init <- AOC.dayInputP 7 WhaleDefense.parsePositions
  printE init
  printE $ WhaleDefense.totalFuel1 <$> init
  printE $ WhaleDefense.totalFuel2 <$> init