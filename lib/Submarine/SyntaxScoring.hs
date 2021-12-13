{-# LANGUAGE LambdaCase #-}

module Submarine.SyntaxScoring where

import Data.List (foldl', sort)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple (swap)
import Flow
import Text.Megaparsec (many, oneOf)
import Text.Megaparsec.Extras (Parser, linesOf)

parseLines :: Parser [String]
parseLines = linesOf (many (oneOf parenChars))

solvePart1 :: [String] -> Int
solvePart1 input =
  mapMaybe (firstCorruption . suggestions) input
    |> map (\(CorruptedOn (c, _, _)) -> c)
    |> map (scores Map.!)
    |> sum
 where
  firstCorruption xs =
    filter isCorrupted xs
      |> \case
        [] -> Nothing
        all -> Just (last all)

tmp input = mapMaybe (Just . suggestions) input

solvePart2 :: [String] -> Int
solvePart2 input =
  mapMaybe (findIncomplete . suggestions) input
    |> map (\(Incomplete remainder) -> remainder)
    |> map scoreCompletion
    |> median

median :: Ord a => [a] -> a
median a =
  sort a
    |> reverse
    |> drop (length a `div` 2)
    |> head

scoreCompletion :: String -> Int
scoreCompletion =
  foldl' (\acc x -> 5 * acc + scores2 Map.! x) 0

findIncomplete :: [Suggestion] -> Maybe Suggestion
findIncomplete suggs
  | hasCorrupted suggs = Nothing
  | null suggs = Nothing
  | otherwise = Just (last (filter isIncomplete suggs))

hasCorrupted :: [Suggestion] -> Bool
hasCorrupted = any isCorrupted

suggestions :: String -> [Suggestion]
suggestions str =
  foldl' step ([], []) (zip str [0 ..])
    |> ( \case
          ([], suggs) -> suggs
          (xs, suggs) -> Incomplete (map (openToClose Map.!) xs) : suggs
       )
    |> reverse
 where
  step :: (String, [Suggestion]) -> (Char, Int) -> (String, [Suggestion])
  step (stack, acc) c =
    (nextStack, acc ++ newSuggests)
   where
    (nextStack, newSuggests) = suggestions' stack c
  suggestions' :: String -> (Char, Int) -> (String, [Suggestion])
  suggestions' stack (c, i)
    | c `elem` closeParens = handleCloseParens stack c i
    | otherwise = (c : stack, [])
  handleCloseParens :: String -> Char -> Int -> (String, [Suggestion])
  handleCloseParens [] c i = ("", [CorruptedOn (c, '∅', i)])
  handleCloseParens (lastOpened : xs) c i
    | closeToOpen Map.! c /= lastOpened = (lastOpened : xs, [CorruptedOn (c, openToClose Map.! lastOpened, i)])
    | otherwise = (xs, [])

closeToOpen :: Map.Map Char Char
closeToOpen = Map.fromList $ map swap parens

openToClose :: Map.Map Char Char
openToClose = Map.fromList parens

closeParens :: [Char]
closeParens = map snd parens

openParens :: [Char]
openParens = map snd parens

parens :: [(Char, Char)]
parens =
  [ ('(', ')')
  , ('[', ']')
  , ('{', '}')
  , ('<', '>')
  ]

parenChars :: String
parenChars = concatMap (\(a, b) -> [a, b]) parens

isCorrupted :: Suggestion -> Bool
isCorrupted = \case
  CorruptedOn _ -> True
  _ -> False

isIncomplete :: Suggestion -> Bool
isIncomplete = \case
  Incomplete _ -> True
  _ -> False

scores :: Map.Map Char Int
scores =
  Map.fromList
    [ (')', 3)
    , (']', 57)
    , ('}', 1197)
    , ('>', 25137)
    ]

scores2 :: Map.Map Char Int
scores2 =
  Map.fromList
    [ (')', 1)
    , (']', 2)
    , ('}', 3)
    , ('>', 4)
    ]

data Suggestion
  = CorruptedOn (Char, Char, Int)
  | Incomplete String
  | InvalidChar
  deriving (Eq, Show)
