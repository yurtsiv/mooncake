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

parseString :: Parser MCValue
parseString = do
   char '"'
   x <- many $ escapedChars <|> many1 (noneOf ['"', '\\'])
   char '"'
   return $ String (concat x)

parseInt :: Parser MCValue
parseInt = do
   digits <- many1 digit
   return $ (Integer . read) digits

parseBool :: Parser MCValue
parseBool = do
   bool <- (string "True") <|> (string "False")
   return $ case bool of
      "True" -> Bool True
      "False" -> Bool False

parseListItemLiteral :: Parser ListItem
parseListItemLiteral = do
   val <- parseMCValue
   return $ Literal val

parseListItemIdentifier :: Parser ListItem
parseListItemIdentifier = do
   id <- parseIdentifier
   return $ Identifier id

parseListItem :: Parser ListItem
parseListItem = 
   try parseListItemIdentifier
   <|> parseListItemLiteral

parseInnerListItem :: Parser ListItem
parseInnerListItem = do
   spaces
   item <- parseListItem
   spaces
   char ','
   return item 

parseLastListItem :: Parser ListItem
parseLastListItem = do
   spaces
   item <- parseListItem
   spaces
   optional $ char ','
   return item

parseEmptyList :: Parser MCValue
parseEmptyList = do
   char '['
   spaces
   char ']'
   return $ List []

parseSingleItemList :: Parser MCValue
parseSingleItemList = do
   char '['
   spaces
   item <- parseLastListItem
   spaces
   char ']'
   return $ List [item]

parseManyItemsList :: Parser MCValue
parseManyItemsList = do
   char '['
   init <- many1 (try parseInnerListItem)
   last <- parseLastListItem
   char ']'
   return $ List (init ++ [last])

parseList :: Parser MCValue
parseList =
   try parseEmptyList
   <|> try parseSingleItemList
   <|> parseManyItemsList


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

parseMCValue :: Parser MCValue
parseMCValue =
   try parseString
   <|> try parseInt
   <|> try parseBool
   <|> try parseList

parseValDeclaration :: Parser AST
parseValDeclaration = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   val <- parseMCValue
   return $ ValDeclaration identifier val

programmParser :: Parser AST
programmParser = do
   spaces
   vars <- many $ do
      var <- parseValDeclaration
      spaces
      return var
   return $ Programm vars