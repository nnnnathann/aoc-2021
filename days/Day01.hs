module Main where

import AOC
import Submarine.Sonar as Sonar
import Text.Megaparsec.Extras (intP, linesOf)

main :: IO ()
main = do
  day1 <- AOC.dayInputP 1 Sonar.parseReport
  printE day1
