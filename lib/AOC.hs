module AOC where

{-
  Functions supporting the overall
  structure of advent of code challenges,
  for instance reading standardized paths
  of input files, and printing output.
-}

import Control.Exception (throw)
import Data.Text (Text, pack)
import Submarine.Controls (NavCmd (..))
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (Parser)
import Text.Printf (printf)

-- Read a day file from day number
dayInput :: Int -> IO Text
dayInput day = do
  input <- readFile $ dayFile day
  return $ pack input

-- Generate a standardized input file path from a day number
dayFile :: Int -> String
dayFile day = "inputs/day" <> printf "%02d" day <> ".txt"

-- Read a day input file and run it through
-- the given parser
dayInputP :: Int -> Parser a -> IO (Either String a)
dayInputP day parser = do
  content <- dayInput day
  let result = parse parser (dayFile day) content
  return $ case result of
    Left err -> Left $ show err
    Right x -> Right x

-- Union output of an either
printE :: (Show e, Show a) => Either e a -> IO ()
printE = either print print