module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Expr
import Data.List

import Parser.AST
import Parser.Utils
import Parser.Language

parseProgramm :: String -> Either ParseError Expression
parseProgramm programm =
   parse programmParser "MoonCake" $ "{" ++ programm ++ "}"

parseString :: Parser Expression
parseString = do
   s <- stringLiteral
   return $ String s

parseInt :: Parser Expression
parseInt = do
   i <- integer
   return $ Integer i

parseBool :: Parser Expression
parseBool =
       Bool True <$ reserved "True"
   <|> Bool False <$ reserved "False"

listItemSep :: Parser ()
listItemSep = do
   whiteSpace
   char ','
   whiteSpace

parseList :: Parser Expression
parseList = do
   char '['
   whiteSpace
   items <- parseExpression `sepEndBy` (try listItemSep)
   whiteSpace
   char ']'
   return $ List items

parseFunction :: Parser Expression
parseFunction = do
   char '('
   whiteSpace
   args <- identifier `sepEndBy` (try listItemSep)
   whiteSpace
   char ')'
   hSpaces 
   reservedOp "->"
   body <- parseExpression
   return $ Function args body

parseIdentifier :: Parser Expression
parseIdentifier = do
   id <- identifier
   return $ Identifier id

parseLet :: Parser Expression
parseLet = do 
   reserved "let"
   id <- identifier
   reservedOp "="
   expr <- parseExpression
   return $ Let id expr

parseBlock :: Parser Expression
parseBlock = do
   whiteSpace
   char '{'
   exprs <- many $ do
      whiteSpace
      expr <- parseExpression
      whiteSpace
      return expr
   char '}'
   return $ Block exprs


binaryOp name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefixOp name fun = Prefix (do{ reservedOp name; return fun })

algOpsTable =
   [ 
     [ binaryOp "*" (Mul) AssocLeft
     , binaryOp "/" (Div) AssocLeft
     , binaryOp "%" (Modulo) AssocLeft
     ]
   , [ binaryOp "+" (Add) AssocLeft
     , binaryOp "-" (Sub) AssocLeft
     ]
   ]

algOpTerm :: Parser Expression
algOpTerm =
   parens parseAlgOperators
   <|> parseIdentifier
   <|> parseInt

parseAlgOperators = buildExpressionParser algOpsTable algOpTerm

logicalOpsTable =
   [ 
     [prefixOp "!" (Inverse)]
   , [ binaryOp ">" (Gt) AssocLeft
     , binaryOp ">=" (GtE) AssocLeft
     , binaryOp "<" (Lt) AssocLeft
     , binaryOp "<" (LtE) AssocLeft
     ]
   , [ binaryOp "==" (Eq) AssocLeft
     ]
   ]

logicalOpTerm :: Parser Expression
logicalOpTerm =
   parens parseLogicalOperators
   <|> parseIdentifier
   <|> parseBool
   <|> parseInt
   <|> parseString
   <|> parseList

parseLogicalOperators = buildExpressionParser logicalOpsTable logicalOpTerm

parseExpression :: Parser Expression
parseExpression =
   try parseAlgOperators
   <|> try parseLogicalOperators
   <|> try parseBlock
   <|> try parseIdentifier
   <|> try parseString
   <|> try parseInt
   <|> try parseBool
   <|> try parseList
   <|> try parseFunction
   <|> try parseLet


programmParser :: Parser Expression
programmParser = do
   programm <- parseExpression
   eof
   return programm