module Main where

import AOC
import Submarine.Controls as Controls

main :: IO ()
main = do
  day2 <- AOC.dayInputP 2 Controls.parsePlan
  putStrLn "Plan 1 Result:"
  AOC.printE $ Controls.runPlanv1 <$> day2
  putStrLn "Plan 2 Result:"
  AOC.printE $ Controls.runPlanv2 <$> day2