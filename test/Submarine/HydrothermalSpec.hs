{-# LANGUAGE QuasiQuotes #-}

module Submarine.HydrothermalSpec where

import Submarine.Hydrothermal
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (ParseResult)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "Submarine.Hydrothermal" $ do
    it "should read segments" $ do
      length <$> exampleInput `shouldBe` Right 10
      head <$> exampleInput `shouldBe` Right ((0, 9), (5, 9))
    it "should calculate intersections" $ do
      hotspotCount1 <$> exampleInput `shouldBe` Right 5
      hotspotCount2 <$> exampleInput `shouldBe` Right 12

exampleInput :: ParseResult [Line]
exampleInput =
  parse
    parseMap
    "Example"
    [r|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2|]