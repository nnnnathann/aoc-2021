module Text.Megaparsec.Extras where

import Control.Monad (void)
import Data.Text as Text
import Data.Void
import Text.Megaparsec (Parsec, many, oneOf, parseTest, sepBy, (<?>))
import Text.Megaparsec.Char (digitChar, newline, space, space1, string)

type Parser = Parsec Void Text

intP :: Parser Int
intP = read <$> many digit

digit :: Parser Char
digit = oneOf ['0' .. '9'] <?> "digit"

spaces :: Parser ()
spaces = void $ oneOf [' ', '\n', '\r', '\t']

linesOf :: Parser a -> Parser [a]
linesOf f = sepBy f newline