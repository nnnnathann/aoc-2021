module Submarine.LanternfishSpec where

import Submarine.Lanternfish
import Test.Hspec

spec :: Spec
spec = do
  describe "lanternfish" $ do
    it "should count after n days" $ do
      countAfterDaysN 80 exampleInput `shouldBe` 5934
      countAfterDaysN 256 exampleInput `shouldBe` 26984457539

exampleInput :: [Int]
exampleInput = [3, 4, 3, 1, 2]
