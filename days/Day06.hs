module Main where

import AOC
import Submarine.Lanternfish
import qualified Submarine.Lanternfish as Lanternfish

main :: IO ()
main = do
  init <- AOC.dayInputP 6 Lanternfish.parseSweep
  printE $ Lanternfish.countAfterDaysN 80 <$> init
  printE $ Lanternfish.countAfterDaysN 256 <$> init
