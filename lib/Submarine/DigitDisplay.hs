module Submarine.DigitDisplay (
  parseRecordings,
  sumDerived,
  countKnown,
) where

import Control.Monad (msum)
import Data.List (find, permutations, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Flow ((|>))
import Text.Megaparsec (MonadParsec (notFollowedBy), many, oneOf, sepBy, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Extras (Parser)

{-
  the format is something like this:
  be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
  edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
-}
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

-- wires are labelled a through g based on the 7 bit display
-- in the readme (top to bottom, left to right)
wires :: String
wires = ['a' .. 'g']

-- map "on" bit positions to their displayed ints
wireDisplay :: Map String Int
wireDisplay =
  Map.fromList $
    zip ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"] [0 .. 9]

-- derive the values of the decoded outputs, and sum them
sumDerived :: [([String], [String])] -> Int
sumDerived entries =
  decodeAll entries
    |> map intListToInt
    |> sum
 where
  intListToInt :: [Int] -> Int
  intListToInt =
    foldl (\acc x -> acc * 10 + x) 0

-- count the values that are "known" (i.e. have a distinct number of "on" bits)
countKnown :: [([String], [String])] -> Int
countKnown entries =
  decodeAll entries
    |> concat
    |> filter (`elem` [1, 4, 7, 8])
    |> length

-- decode a list of entries, discarding all failures
decodeAll :: [([String], [String])] -> [[Int]]
decodeAll = mapMaybe decodeEntry

-- accepts a single entry from the recorded logs,
-- and searches the possible rewirings for one
-- that produces recognized integers
decodeEntry :: ([String], [String]) -> Maybe [Int]
decodeEntry entry =
  map (Map.fromList . zip wires) (permutations wires)
    |> map (decodeEntryWith entry)
    |> firstJust
 where
  -- if all the signals are valid digits (after translation),
  -- decode the outputs. otherwise return Nothing
  decodeEntryWith :: ([String], [String]) -> Map Char Char -> Maybe [Int]
  decodeEntryWith (signals, outputs) key =
    traverse (rewireWith key) signals *> traverse (rewireWith key) outputs
  -- rewire a wire rep to its "decoded" integer
  rewireWith :: Map Char Char -> String -> Maybe Int
  rewireWith key c = Map.lookup (translateWith key c) wireDisplay
  -- translate a wire rep to its "decoded" state
  translateWith :: Map Char Char -> String -> String
  translateWith key c = sort (map (key Map.!) c)

-- there must be a better way, but, dunno.
firstJust :: [Maybe a] -> Maybe a
firstJust =
  msum