module Main where

import AOC
import qualified Submarine.SyntaxScoring as SyntaxScoring

main :: IO ()
main = do
  init <- AOC.dayInputP 10 SyntaxScoring.parseLines
  printE $ SyntaxScoring.solvePart1 <$> init
  printE $ SyntaxScoring.solvePart2 <$> init