module Parser.Language where

import Text.Parsec.Char
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

builtInFunctions = ["len"]

reservedNames =
  [ "let",
    "True",
    "False",
    "if",
    "else",
    "else if",
    "do",
    "end",
    "then",
    "while"
  ] ++ builtInFunctions

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
    "||",
    "&&",
    "++",
    "/="
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

float = T.float lexer

whiteSpace = T.whiteSpace lexer