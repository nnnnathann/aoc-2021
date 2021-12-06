{-# LANGUAGE QuasiQuotes #-}

module Submarine.Bingo (
  Game,
  parseGame,
  scoreFirstWinner,
  scoreLastWinner,
) where

import Control.Monad (void)
import Data.Foldable (any, maximumBy)
import Data.Function (on)
import qualified Data.IntSet as ISet
import Data.List (findIndex, minimumBy, sortBy, transpose)
import qualified Data.Set as Set
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), count, many, manyTill, parse, parseTest, sepBy, sepEndBy, sepEndBy1, (<|>))
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Extras (Parser, digit, intP, spaces)
import Text.RawString.QQ

-- Parsing

type Game = ([Int], [[[Int]]])

parseGame :: Parser Game
parseGame = do
  numbers <- sepBy intP (char ',')
  _ <- blankLine
  boards <- sepBy boardP blankLine
  return (numbers, boards)

blankLine :: Parser ()
blankLine = void $ count 2 newline

boardP :: Parser [[Int]]
boardP =
  sepBy boardLineP (try (newline <* notFollowedBy newline))

boardLineP :: Parser [Int]
boardLineP = do
  _ <- spaces
  sepEndBy1 intP spaces

-- Board calculations

scoreFirstWinner :: Game -> Maybe Int
scoreFirstWinner game = do
  winner <- firstWinner game
  return $ scoreBoard game winner

scoreLastWinner :: Game -> Maybe Int
scoreLastWinner game = do
  winner <- lastWinner game
  return $ scoreBoard game winner

scoreBoard :: Game -> (Int, [[Int]]) -> Int
scoreBoard game (pos, board) =
  sumOfNonCalled * wonAt
 where
  wonAt = fst game !! pos
  called = ISet.fromList (take (pos + 1) (fst game))
  sumOfNonCalled = sum $ filter (\a -> not (ISet.member a called)) $ concat board

firstWinner :: Game -> Maybe (Int, [[Int]])
firstWinner game =
  case minimumBy (compare `on` fst) (winningPositions game) of
    (Nothing, _) -> Nothing
    (Just pos, board) -> Just (pos, board)

lastWinner :: Game -> Maybe (Int, [[Int]])
lastWinner game =
  case maximumBy (compare `on` fst) (winningPositions game) of
    (Nothing, _) -> Nothing
    (Just pos, board) -> Just (pos, board)

winningPositions :: Game -> [(Maybe Int, [[Int]])]
winningPositions (numbers, boards) =
  map (winningIndex numbers) boards

winningIndex :: [Int] -> [[Int]] -> (Maybe Int, [[Int]])
winningIndex numbers board =
  (findIndex hasWinner (numberSets numbers), board)
 where
  hasWinner :: ISet.IntSet -> Bool
  hasWinner numbers = any (`ISet.isSubsetOf` numbers) (winningSets board)

numberSets :: [Int] -> [ISet.IntSet]
numberSets numbers = map (\i -> ISet.fromList (take i numbers)) [1 .. (length numbers)]

winningSets :: [[Int]] -> Set.Set ISet.IntSet
winningSets board =
  Set.fromList $ map ISet.fromList board ++ map ISet.fromList (transpose board)
