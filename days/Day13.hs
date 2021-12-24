module Main where

import AOC
import qualified Submarine.TransparentOragami as TransparentOragami

main :: IO ()
main = do
  init <- AOC.dayInputP 13 TransparentOragami.parseInstructions
  printE $ TransparentOragami.countVisibleDots <$> init