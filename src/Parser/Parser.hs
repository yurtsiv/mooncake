module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.List

import Parser.AST
import Parser.Utils
import Parser.Language

parseProgramm :: String -> Either ParseError Programm
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
   body <- parseCodeBlock
   return $ Function args body

parseExprIdentifier :: Parser Expression
parseExprIdentifier = do
   id <- identifier
   return $ Identifier id

parseLet :: Parser Expression
parseLet = do 
   reserved "let"
   id <- identifier
   reservedOp "="
   expr <- parseExpression
   return $ Let id expr

parseExpression :: Parser Expression
parseExpression =
   try parseString
   <|> try parseInt
   <|> try parseBool
   <|> try parseList
   <|> try parseFunction
   <|> try parseLet
   <|> try parseExprIdentifier

parseCodeBlock :: Parser Programm
parseCodeBlock = do
   whiteSpace
   char '{'
   programm <- many $ do
      whiteSpace
      comp <- parseExpression
      whiteSpace
      return comp
   char '}'
   return programm

programmParser :: Parser Programm
programmParser = do
   programm <- parseCodeBlock
   eof
   return programm