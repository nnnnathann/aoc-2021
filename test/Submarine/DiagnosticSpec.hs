{-# LANGUAGE QuasiQuotes #-}

module Submarine.DiagnosticSpec where

import Submarine.Diagnostic
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (ParseResult)
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Submarine.Diagnostic" $ do
    it "should report power consumption" $ do
      powerConsumption <$> testInput `shouldBe` Right 198
      lifeSupport <$> testInput `shouldBe` Right (Just 230)

testInput :: ParseResult [[Bit]]
testInput =
  parse
    parseReport
    "Test Input"
    [r|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010|]