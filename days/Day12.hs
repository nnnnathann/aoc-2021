module Main where

import AOC
import qualified Submarine.PassagePathing as PassagePathing

main :: IO ()
main = do
  init <- AOC.dayInputP 12 PassagePathing.parseCaves
  printE $ PassagePathing.countPaths PassagePathing.AtMostOnce <$> init
  printE $ PassagePathing.countPaths PassagePathing.SingleSmallCaveTwice <$> init