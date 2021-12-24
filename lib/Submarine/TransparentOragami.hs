{-# LANGUAGE TupleSections #-}

module Submarine.TransparentOragami where

import Control.Applicative (Applicative (liftA2))
import Text.Megaparsec (choice, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Extras (Parser, blankLine, intP, linesOf)

type Input = ([(Int, Int)], [Instruction])

data Instruction
  = FoldX Int
  | FoldY Int
  deriving (Show, Eq)

parseInstructions :: Parser Input
parseInstructions = do
  dots <- linesOf (liftA2 (,) (intP <* char ',') intP)
  _ <- blankLine
  instructions <- linesOf instruction
  pure (dots, instructions)
 where
  instruction = foldX <|> foldY
  foldX = do
    _ <- string "fold along x="
    FoldX <$> intP
  foldY = do
    _ <- string "fold along y="
    FoldY <$> intP

countVisibleDots :: Input -> [Int]
countVisibleDots (dots, instructions) =
  map (const 1) instructions