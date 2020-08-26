module Parser.Utils where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

-- horizontal white space 
hWhiteSpace :: Parser ()
hWhiteSpace = skipMany $ oneOf [' ', '\t']
