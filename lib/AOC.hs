module AOC where

import Control.Exception (throw)
import Data.Text (Text, pack)
import Submarine.Controls (NavCmd (..))
import Text.Megaparsec (parse)
import Text.Megaparsec.Extras (Parser)
import Text.Printf (printf)

dayInput :: Int -> IO Text
dayInput day = do
  input <- readFile $ dayFile day
  return $ pack input

dayFile :: Int -> String
dayFile day = "inputs/day" <> printf "%02d" day <> ".txt"

dayInputP :: Int -> Parser a -> IO (Either String a)
dayInputP day parser = do
  content <- dayInput day
  let result = parse parser (dayFile day) content
  return $ case result of
    Left err -> Left $ show err
    Right x -> Right x

printE :: (Show e, Show a) => Either e a -> IO ()
printE = either print print