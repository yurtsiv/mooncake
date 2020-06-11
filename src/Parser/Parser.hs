module Parser.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Parser.AST (MCValue(..), AST(..))

readProgramm :: String -> String
readProgramm input = case parse parseProgram "MoonCake" input of
  Left err -> "Error: " ++ show err
  Right val -> show val

parseIdentifier :: Parser String
parseIdentifier = many1 letter

parseString :: Parser MCValue
parseString = do char '"'
                 x <- many $ escapedChars <|> many1 (noneOf ['"', '\\'])
                 char '"'
                 return $ String (concat x)

escapedChars :: Parser String
escapedChars = do char '\\'
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
parseVariableDeclaration = do string "let "
                              identifier <- parseIdentifier
                              string " = "
                              val <- parseMCValue
                              return $ ValueDeclaration identifier val

parseProgram :: Parser AST
parseProgram = parseVariableDeclaration