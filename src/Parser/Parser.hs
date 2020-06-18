module Parser.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.AST (MCValue(..), AST(..))

parseProgramm = parse programmParser "MoonCake"

parseIdentifier :: Parser String
parseIdentifier = many1 letter

parseString :: Parser MCValue
parseString = do
   char '"'
   x <- many $ escapedChars <|> many1 (noneOf ['"', '\\'])
   char '"'
   return $ String (concat x)

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
parseMCValue = parseString

parseVariableDeclaration :: Parser AST
parseVariableDeclaration = do 
   string "let "
   identifier <- parseIdentifier
   string " = "
   val <- parseMCValue
   return $ ValueDeclaration identifier val

programmParser :: Parser AST
programmParser = do
   skipMany $ oneOf ['\n', '\t', ' ']
   var <- parseVariableDeclaration
   return var