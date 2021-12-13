{-# LANGUAGE LambdaCase #-}

module Submarine.SyntaxScoringSpec where

import Submarine.SyntaxScoring

import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowM)
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (ParseResult)
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Syntax scoring" $ do
    it "should score a simple expression" $ do
      (last . filter isCorrupted) (suggestions "{([(<{}[<>[]}>{[]{[(<()>") `shouldBe` CorruptedOn ('}', ']', 12)
      (last . filter isCorrupted) (suggestions "[[<[([]))<([[{}[[()]]]") `shouldBe` CorruptedOn (')', ']', 8)
      (last . filter isCorrupted) (suggestions "[{[{({}]{}}([{[{{{}}([]") `shouldBe` CorruptedOn (']', ')', 7)
    it "should solve part 1" $ do
      solvePart1 <$> testInput `shouldBe` Right 26397
    it "should solve part 2" $ do
      scoreCompletion "])}>" `shouldBe` 294
      length . mapMaybe (findIncomplete . suggestions) <$> testInput `shouldBe` Right 5
      solvePart2 <$> testInput `shouldBe` Right 288957

testInput :: ParseResult [String]
testInput =
  parse
    parseLines
    "example"
    [r|[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]|]