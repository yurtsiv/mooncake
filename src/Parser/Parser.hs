module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Parser.AST
import Parser.Utils

parseProgramm = parse programmParser "MoonCake"

parseIdentifier :: Parser String
parseIdentifier = do
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

parseReferenceValue :: Parser Reference
parseReferenceValue = do
   val <- parseLiteral
   return $ Value val

parseReferenceIdentifier :: Parser Reference
parseReferenceIdentifier = do
   id <- parseIdentifier
   return $ Identifier id

parseReference :: Parser Reference
parseReference = 
   try parseReferenceIdentifier
   <|> parseReferenceValue

listItemSep :: Parser ()
listItemSep = do
   spaces
   char ','
   spaces

parseList :: Parser Literal
parseList = do
   char '['
   spaces
   items <- parseReference `sepEndBy` (try listItemSep)
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

parseLiteral :: Parser Literal
parseLiteral =
   try parseString
   <|> try parseInt
   <|> try parseBool
   <|> try parseList

parseDeclaration :: Parser Component
parseDeclaration = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   ref <- parseReference
   return $ VarDeclaration identifier ref

parseComment :: Parser Component
parseComment = do
   hSpaces
   char '#'
   comment <- manyTill anyChar (try (char '\n'))
   return Noop

parseComponent :: Parser Component
parseComponent =
   try parseDeclaration
   <|> try parseComment

programmParser :: Parser Programm
programmParser = do
   comps <- many $ do
      newlines
      comp <- parseComponent
      newlines
      return comp

   eof
   return comps