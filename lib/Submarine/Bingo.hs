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

testScoring :: IO ()
testScoring = do
  let winners = firstWinner <$> exampleInput
  let a = take 12 . fst <$> exampleInput
  print a

winningPositions :: Game -> [(Maybe Int, [[Int]])]
winningPositions (numbers, boards) =
  map (winningIndex numbers) boards

winningIndex :: [Int] -> [[Int]] -> (Maybe Int, [[Int]])
winningIndex numbers board =
  (findIndex hasWinner (numberSets numbers), board)
 where
  winners :: Set.Set ISet.IntSet
  winners = winningSets board

  hasWinner :: ISet.IntSet -> Bool
  hasWinner numbers = any (`ISet.isSubsetOf` numbers) winners

numberSets :: [Int] -> [ISet.IntSet]
numberSets numbers = map (\i -> ISet.fromList (take i numbers)) [1 .. (length numbers)]

winningSets :: [[Int]] -> Set.Set ISet.IntSet
winningSets board =
  Set.fromList $ map ISet.fromList board ++ map ISet.fromList (transpose board)

exampleInput =
  parse
    parseGame
    "example"
    [r|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7|]

exampleBoard =
  parse
    (sepBy boardP blankLine)
    "exampleBoard"
    [r|22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7|]