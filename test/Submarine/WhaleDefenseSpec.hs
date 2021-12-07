module Submarine.WhaleDefenseSpec where

import Submarine.WhaleDefense
import Test.Hspec

spec :: Spec
spec = do
  describe "solving" $ do
    it "find min fuel" $ do
      totalFuel1 [16, 1, 2, 0, 4, 2, 7, 1, 2, 14] `shouldBe` 37
      totalFuel2 [16, 1, 2, 0, 4, 2, 7, 1, 2, 14] `shouldBe` 168