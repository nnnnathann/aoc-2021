{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Submarine.PassagePathingSpec where

import Data.Either (fromRight)
import Data.List (sort)
import Data.Set (powerSet)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
import Flow ((|>))
import Submarine.PassagePathing
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (ParseResult)
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "passage pathing" $ do
    it "should solve part 1" $ do
      runTest (countPaths AtMostOnce) inputSample1 `shouldBe` 10
      runTest (countPaths AtMostOnce) inputSample2 `shouldBe` 19
      runTest (countPaths AtMostOnce) inputSample3 `shouldBe` 226
    it "should solve part 2" $ do
      runTest (countPaths SingleSmallCaveTwice) inputSample1 `shouldBe` 36
      runTest (countPaths SingleSmallCaveTwice) inputSample2 `shouldBe` 103
      runTest (countPaths SingleSmallCaveTwice) inputSample3 `shouldBe` 3509

runTest :: ([Passage] -> Int) -> Text -> Int
runTest f input =
  parse parseCaves "runTest" input
    |> fmap f
    |> fromRight 0

sample1 :: [Passage]
sample1 = mustSample inputSample1

sample2 :: [Passage]
sample2 = mustSample inputSample2

mustSample :: Text -> [Passage]
mustSample input =
  parse parseCaves "mustSample" input
    |> fromRight []

inputSample1 :: Text
inputSample1 =
  [r|start-A
start-b
A-c
A-b
b-d
A-end
b-end|]

inputSample2 :: Text
inputSample2 =
  [r|dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc|]

inputSample3 :: Text
inputSample3 =
  [r|fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW|]