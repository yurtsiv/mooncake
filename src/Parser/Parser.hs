module Parser.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char
import Parser.AST

parseProgramm = parse programmParser "MoonCake"

parseIdentifier :: Parser String
parseIdentifier = do
   head <- letter
   rest <- many (digit <|> letter)
   return $ [head] ++ rest

parseString :: Parser MCLiteral
parseString = do
   char '"'
   x <- many $ escapedChars <|> many1 (noneOf ['"', '\\'])
   char '"'
   return $ String (concat x)

parseInt :: Parser MCLiteral
parseInt = do
   sign <- option '+' (char '-')
   digits <- many1 digit
   return $ case sign of
      '+' -> (Integer . read) digits
      '-' -> (Integer . (* (-1)) . read) digits

parseBool :: Parser MCLiteral
parseBool = do
   bool <- (string "True") <|> (string "False")
   return $ case bool of
      "True" -> Bool True
      "False" -> Bool False

parseReferenceLiteral :: Parser Reference
parseReferenceLiteral = do
   val <- parseMCLiteral
   return $ Literal val

parseReferenceIdentifier :: Parser Reference
parseReferenceIdentifier = do
   id <- parseIdentifier
   return $ Identifier id

parseReference :: Parser Reference
parseReference = 
   try parseReferenceIdentifier
   <|> parseReferenceLiteral

listItemSep :: Parser ()
listItemSep = do
   spaces
   char ','
   spaces

parseList :: Parser MCLiteral
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

parseMCLiteral :: Parser MCLiteral
parseMCLiteral =
   try parseString
   <|> try parseInt
   <|> try parseBool
   <|> try parseList

parseVarDeclaration :: Parser AST
parseVarDeclaration = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   ref <- parseReference
   return $ VarDeclaration identifier ref

programmParser :: Parser Programm
programmParser = do
   spaces
   vars <- many $ do
      var <- parseVarDeclaration
      spaces
      return var
   return vars