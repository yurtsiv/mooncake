module Parser.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.AST (MCValue(..), AST(..))

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

parseValDeclaration :: Parser AST
parseValDeclaration = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   val <- parseMCValue
   return $ ValDeclaration identifier val

programmParser :: Parser AST
programmParser = do
   skipMany $ oneOf ['\n', '\t', ' ']
   vars <- many $ do
      var <- parseValDeclaration
      char '\n'
      return var
   return $ Programm vars