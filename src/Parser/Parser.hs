module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Data.List

import Parser.AST
import Parser.Utils

parseProgramm = parse programmParser "MoonCake"

parseReservedNames = do
   string "let"

parseIdentifier :: Parser String
parseIdentifier = do
   notFollowedBy parseReservedNames
   head <- letter
   rest <- many (digit <|> letter)
   return $ [head] ++ rest

parseString :: Parser Literal
parseString = do
   char '"'
   x <- many $ escapedChars <|> many1 (noneOf ['"', '\\'])
   char '"'
   return $ String (concat x)

parseInt :: Parser Literal
parseInt = do
   sign <- option '+' (char '-')
   digits <- many1 digit
   return $ case sign of
      '+' -> (Integer . read) digits
      '-' -> (Integer . (* (-1)) . read) digits

parseBool :: Parser Literal
parseBool = do
   bool <- (string "True") <|> (string "False")
   return $ case bool of
      "True" -> Bool True
      "False" -> Bool False

parseReferenceValue :: Int -> Parser Reference
parseReferenceValue level = do
   val <- parseLiteral level
   return $ Value val

parseReferenceIdentifier :: Parser Reference
parseReferenceIdentifier = do
   id <- parseIdentifier
   return $ Identifier id

parseReference :: Int -> Parser Reference
parseReference level = do
   try parseReferenceIdentifier
   <|> (parseReferenceValue level)

listItemSep :: Parser ()
listItemSep = do
   spaces
   char ','
   spaces

parseList :: Int -> Parser Literal
parseList level = do
   char '['
   spaces
   items <- (parseReference level) `sepEndBy` (try listItemSep)
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

parseFunction :: Int -> Parser Literal
parseFunction level = do
   char '('
   spaces
   args <- parseIdentifier `sepEndBy` (try listItemSep)
   spaces
   string ") ->"
   body <- parseCodeBlock (level + 1)
   return $ Function args body

parseLiteral :: Int -> Parser Literal
parseLiteral level =
   try parseString
   <|> try parseInt
   <|> try parseBool
   <|> (try $ parseList level)
   <|> (try $ parseFunction level)

parseDeclaration :: Int -> Parser Component
parseDeclaration level = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   ref <- parseReference level
   return $ Declaration identifier ref

parseComment :: Parser Component
parseComment = do
   hSpaces
   char '#'
   comment <- manyTill anyChar (try (char '\n'))
   return Noop

parseComponentReference :: Int -> Parser Component
parseComponentReference level = do
   ref <- parseReference level
   return $ Reference ref

parseComponent :: Int -> Parser Component
parseComponent level = do
   try $ parseDeclaration level
   <|> (try $ parseComponentReference level)
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