{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Submarine.OctopusSpec where

import Submarine.Octopus

import Data.Grid (coords)
import Debug.Trace (trace, traceShowM)
import Test.Hspec
import Text.Megaparsec (parse, sepBy)
import Text.Megaparsec.Extras (ParseResult, blankLine)
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Octopus" $ do
    it "should progress" $ do
      (!! 1) . simulate <$> testInput1 `shouldBe` coords . (!! 1) <$> testInput1Results
      solvePart1 <$> testInput `shouldBe` Right 1656
      solvePart2 <$> testInput `shouldBe` Right 195

testInput1 :: ParseResult [[Int]]
testInput1 =
  parse
    parseGrid
    "example1"
    [r|11111
19991
19191
19991
11111|]

testInput1Results :: ParseResult [[[Int]]]
testInput1Results =
  parse
    (sepBy parseGrid blankLine)
    "examples"
    [r|34543
40004
50005
40004
34543

45654
51115
61116
51115
45654|]

testInput2 :: ParseResult [[Int]]
testInput2 =
  parse
    parseGrid
    "example2"
    [r|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|]

testInput :: ParseResult [[Int]]
testInput =
  parse
    parseGrid
    "example"
    [r|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526|]