{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Submarine.TransparentOragamiSpec where

import Data.Either (fromRight)
import Data.List (sort)
import Data.Set (powerSet)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
import Flow ((|>))
import Submarine.TransparentOragami
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (ParseResult)
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "passage pathing" $ do
    it "should solve part 1" $ do
      traceShowM sample1
      head (countVisibleDots sample1) `shouldBe` 17

sample1 :: Input
sample1 = mustSample inputSample1

mustSample :: Text -> Input
mustSample input =
  parse parseInstructions "mustSample" input
    |> fromRight ([], [])

inputSample1 :: Text
inputSample1 =
  [r|6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5|]
