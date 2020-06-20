module Parser.Utils where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

hSpace :: Parser Char
hSpace = oneOf [' ', '\t']

hSpaces :: Parser ()
hSpaces = skipMany hSpace

newlines :: Parser ()
newlines = skipMany newline