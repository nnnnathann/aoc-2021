module Main where

import AOC
import qualified Submarine.Octopus as Octopus

main :: IO ()
main = do
  init <- AOC.dayInputP 11 Octopus.parseGrid
  printE $ Octopus.solvePart1 <$> init
  printE $ Octopus.solvePart2 <$> init