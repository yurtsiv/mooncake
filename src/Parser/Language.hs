module Parser.Language where

import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

reservedNames =
  [ "let",
    "True",
    "False",
    "if",
    "else"
  ]

reservedOpNames =
  [ "+",
    "-",
    "*",
    "/",
    "%",
    "==",
    ">",
    ">=",
    "<",
    "<=",
    "!",
    "++"
  ]

moonCakeDef =
  emptyDef
    { T.commentStart = "",
      T.commentEnd = "",
      T.commentLine = "#",
      T.nestedComments = False,
      T.identStart = letter,
      T.identLetter = alphaNum,
      T.reservedNames = reservedNames,
      T.reservedOpNames = reservedOpNames
    }

lexer = T.makeTokenParser moonCakeDef

identifier = T.identifier lexer

parens = T.parens lexer

stringLiteral = T.stringLiteral lexer

reserved = T.reserved lexer

reservedOp = T.reservedOp lexer

integer = T.integer lexer

whiteSpace = T.whiteSpace lexer