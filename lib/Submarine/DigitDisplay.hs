module Submarine.DigitDisplay where

import Data.List (find, permutations, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Flow ((|>))
import Text.Megaparsec (MonadParsec (notFollowedBy), many, oneOf, sepBy, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Extras (Parser)

parseRecordings :: Parser [([String], [String])]
parseRecordings =
  sepBy parseRecording (char '\n')
 where
  wireP :: Parser String
  wireP = sort <$> many (oneOf wires)

  parseRecording :: Parser ([String], [String])
  parseRecording = do
    signals <- sepBy wireP (try (char ' ' <* notFollowedBy (char '|')))
    _ <- many (oneOf [' ', '|', '\n'])
    outputs <- sepBy wireP (char ' ')
    return (signals, outputs)

wires :: String
wires = ['a' .. 'g']

wireDisplay :: Map String Int
wireDisplay =
  Map.fromList $
    zip ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"] [0 .. 9]

sumDerived :: [([String], [String])] -> Int
sumDerived entries =
  decodeAll entries
    |> map intListToInt
    |> sum
 where
  intListToInt :: [Int] -> Int
  intListToInt =
    foldl (\acc x -> acc * 10 + x) 0

countKnown :: [([String], [String])] -> Int
countKnown entries =
  decodeAll entries
    |> concat
    |> filter (`elem` [1, 4, 7, 8])
    |> length

decodeAll :: [([String], [String])] -> [[Int]]
decodeAll = mapMaybe findMapping

findMapping :: ([String], [String]) -> Maybe [Int]
findMapping entry =
  map (Map.fromList . zip wires) (permutations wires)
    |> map (decodeEntry entry)
    |> firstJust

decodeEntry :: ([String], [String]) -> Map Char Char -> Maybe [Int]
decodeEntry (signals, outputs) key =
  traverse (rewireWith key) signals *> traverse (rewireWith key) outputs

rewireWith :: Map Char Char -> String -> Maybe Int
rewireWith key c = Map.lookup (translateWith key c) wireDisplay

translateWith :: Map Char Char -> String -> String
translateWith key c = sort (map (key Map.!) c)

firstJust :: [Maybe a] -> Maybe a
firstJust (Just a : xs) = Just a
firstJust (Nothing : xs) = firstJust xs
firstJust [] = Nothing
