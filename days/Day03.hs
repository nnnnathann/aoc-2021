module Main where

import AOC
import Data.Text (Text)
import Submarine.Diagnostic as Diagnostic

main :: IO ()
main = do
  day3 <- AOC.dayInputP 3 Diagnostic.parseReport
  putStrLn "Submarine Diagnostic Report"
  putStrLn "==========================="
  putStrLn "Power Consumption:"
  AOC.printE $ Diagnostic.powerConsumption <$> day3
  putStrLn "Life Support:"
  AOC.printE $ Diagnostic.lifeSupport <$> day3
