{-# LANGUAGE QuasiQuotes #-}

module Submarine.LavaTubesSpec where

import Data.List (sort)
import Submarine.LavaTubes
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (ParseResult)
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "LavaTubes" $ do
    it "should find risk points" $ do
      sumRiskLowPoints <$> testInput `shouldBe` Right 15
    it "should find low points" $ do
      lowPoints testLowPoints `shouldBe` [1]
    it "should find segments" $ do
      sort . map length . basins <$> testInput `shouldBe` Right [3, 9, 9, 14]
    it "should find segment sums" $ do
      take 3 . reverse . sort . map length . basins <$> testInput `shouldBe` Right [14, 9, 9]

testLowPoints :: [[Int]]
testLowPoints =
  [ [9, 2, 3]
  , [4, 1, 6]
  , [7, 8, 9]
  ]

testInput :: ParseResult [[Int]]
testInput =
  parse
    parseHeightMap
    "height map test"
    [r|2199943210
3987894921
9856789892
8767896789
9899965678|]