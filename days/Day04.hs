module Main where

import AOC
import Data.Text (Text)
import Submarine.Bingo as Bingo

main :: IO ()
main = do
  game <- AOC.dayInputP 4 Bingo.parseGame
  case game of
    Left err -> print err
    Right game' -> do
      _ <- showFirstWinner game'
      showLastWinner game'

showFirstWinner :: Bingo.Game -> IO ()
showFirstWinner game = do
  putStrLn "Finding first winner score..."
  case Bingo.scoreFirstWinner game of
    Just winner -> do
      putStrLn $ "Found a winner! The score is: " ++ show winner
    Nothing -> putStrLn "No winner found."

showLastWinner :: Bingo.Game -> IO ()
showLastWinner game = do
  putStrLn "Finding last winner score..."
  case Bingo.scoreLastWinner game of
    Just winner -> do
      putStrLn $ "Found a last winner! The score is: " ++ show winner
    Nothing -> putStrLn "No winner found."