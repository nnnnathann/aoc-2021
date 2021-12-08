module Main where

import AOC
import qualified Submarine.DigitDisplay as DigitDisplay

main :: IO ()
main = do
  init <- AOC.dayInputP 8 DigitDisplay.parseRecordings
  printE $ DigitDisplay.countKnown <$> init
  printE $ DigitDisplay.sumDerived <$> init