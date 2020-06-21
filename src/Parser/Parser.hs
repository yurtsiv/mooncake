module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
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
parseBool = do
   bool <- (string "True") <|> (string "False")
   return $ case bool of
      "True" -> Bool True
      "False" -> Bool False

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
   string "->"
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

parseExpression :: Parser Expression
parseExpression =
   try parseBlock
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