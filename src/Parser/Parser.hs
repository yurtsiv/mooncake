module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.List

import Parser.AST
import Parser.Utils

parseProgramm = parse programmParser "MoonCake"

reservedNames = ["let"]

parseReservedNames :: Parser ()
parseReservedNames = mapM_ string reservedNames

parseIdentifier :: Parser String
parseIdentifier = do
   notFollowedBy parseReservedNames
   head <- letter
   rest <- many (digit <|> letter)
   return $ [head] ++ rest

parseString :: Parser Expression
parseString = do
   char '"'
   x <- many $ escapedChars <|> many1 (noneOf ['"', '\\'])
   char '"'
   return $ String (concat x)

parseInt :: Parser Expression
parseInt = do
   sign <- option '+' (char '-')
   digits <- many1 digit
   return $ case sign of
      '+' -> (Integer . read) digits
      '-' -> (Integer . (* (-1)) . read) digits

parseBool :: Parser Expression
parseBool = do
   bool <- (string "True") <|> (string "False")
   return $ case bool of
      "True" -> Bool True
      "False" -> Bool False

listItemSep :: Parser ()
listItemSep = do
   spaces
   char ','
   spaces

parseList :: Int -> Parser Expression
parseList level = do
   char '['
   spaces
   items <- (parseExpression level) `sepEndBy` (try listItemSep)
   spaces
   char ']'
   return $ List items

escapedChars :: Parser String
escapedChars = do
   char '\\'
   c <- oneOf "'\\nrt"
   return $ case c of
      '\\' -> "\\"
      '\'' -> "'"
      'n' -> "\n"
      'r' -> "\r"
      't' -> "\t"

parseFunction :: Int -> Parser Expression
parseFunction level = do
   char '('
   spaces
   args <- parseIdentifier `sepEndBy` (try listItemSep)
   spaces
   string ") ->"
   body <- parseCodeBlock (level + 1)
   return $ Function args body

parseExprIdentifier :: Parser Expression
parseExprIdentifier = do
   id <- parseIdentifier
   return $ Identifier id

parseExpression :: Int -> Parser Expression
parseExpression level =
   try parseString
   <|> try parseInt
   <|> try parseBool
   <|> (try $ parseList level)
   <|> (try $ parseFunction level)
   <|> try parseExprIdentifier

parseDeclaration :: Int -> Parser Component
parseDeclaration level = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   expr <- parseExpression level
   return $ Declaration identifier expr

parseComment :: Parser Component
parseComment = do
   hSpaces
   char '#'
   comment <- manyTill anyChar (try (char '\n'))
   return Noop

parseComponentExpression :: Int -> Parser Component
parseComponentExpression level = do
   expr <- parseExpression level
   return $ Expression expr

parseComponent :: Int -> Parser Component
parseComponent level = do
   (try $ parseDeclaration level)
   <|> (try $ parseComponentExpression level)
   <|> try parseComment

parseIndentation :: Int -> Parser String
parseIndentation level =
   (try $ string $ replicate level '\t')
   <|> (string $ replicate (level * 2) ' ')

parseCodeBlock :: Int -> Parser Programm
parseCodeBlock level = many $ do
   newlines
   parseIndentation level
   comp <- parseComponent level
   newlines
   return comp

programmParser :: Parser Programm
programmParser = do
   programm <- parseCodeBlock 0
   eof
   return programm