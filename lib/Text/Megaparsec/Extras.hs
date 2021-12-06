module Text.Megaparsec.Extras where

import Control.Monad (void)
import Data.Text as Text
import Data.Void
import Text.Megaparsec (Parsec, many, oneOf, parseTest, sepBy, (<?>))
import qualified Text.Megaparsec as Err
import Text.Megaparsec.Char (char, digitChar, newline, space, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

type ParseResult a = Either (Err.ParseErrorBundle Text.Text Data.Void.Void) a

intP :: Parser Int
intP = decimal

digit :: Parser Char
digit = oneOf ['0' .. '9'] <?> "digit"

whitespace :: Parser ()
whitespace = void $ oneOf [' ', '\n', '\r', '\t']

spaces :: Parser ()
spaces = void $ many (char ' ')

linesOf :: Parser a -> Parser [a]
linesOf f = sepBy f newline