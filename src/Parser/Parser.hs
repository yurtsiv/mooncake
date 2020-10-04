module Parser.Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Expr
import Data.List

import Parser.AST
import Parser.Utils
import Parser.Language

parseProgramm :: String -> Either ParseError Expression
parseProgramm programm =
   parse programmParser "MoonCake" $ "do " ++ programm ++ " end"

parseString :: Parser Expression
parseString = do
   s <- stringLiteral
   return $ String s

parseInt :: Parser Expression
parseInt =  do
   i <- integer
   return $ Integer i

parseChar :: Parser Expression
parseChar = do
   c <- charLiteral
   return $ Char c

parseFloat :: Parser Expression
parseFloat = do
   f <- float
   return $ Float f

parseBool :: Parser Expression
parseBool =
       Bool True <$ reserved "True"
   <|> Bool False <$ reserved "False"

listItemSep :: Parser ()
listItemSep = do
   whiteSpace
   char ','
   whiteSpace

parseList :: Parser Expression
parseList = do
   char '['
   whiteSpace
   items <- parseExpression `sepEndBy` try listItemSep
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
   whiteSpace
   body <- parseBlock
   return $ Function args body

parseBuiltInFunctionName :: Parser String
parseBuiltInFunctionName = choice $ map string builtInFunctions

parseFunctionCall :: Parser Expression
parseFunctionCall = do
   id <- (identifier <|> parseBuiltInFunctionName)
   char '('
   whiteSpace
   args <- parseExpression `sepEndBy` (try listItemSep)
   whiteSpace
   char ')'
   return $ FunctionCall id args

parseIdentifier :: Parser Expression
parseIdentifier = Identifier <$> identifier

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
   reserved "do"
   exprs <- many $ do
      whiteSpace
      expr <- parseExpression
      whiteSpace
      return expr
   reserved "end"
   return $ Block exprs

parseIf :: Parser Expression
parseIf = do
   reserved "if"
   condition <- parseExpression
   reserved "then"
   body <- parseExpression
   reserved "end"
   return $ If condition body

parseIfElse :: Parser Expression
parseIfElse = do
   reserved "if"
   ifCond <- parseExpression
   reserved "then"
   ifBody <- parseExpression
   whiteSpace
   reserved "else"
   elseBody <- parseExpression
   reserved "end"
   return $ IfElse ifCond ifBody elseBody

binaryOp name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefixOp name fun = Prefix (do{ reservedOp name; return fun })

operatorsTable =
   [
     [ prefixOp "-" Negative
     , prefixOp "+" Positive
     ]
   , [ binaryOp "*" Mul AssocRight
     , binaryOp "/" Div AssocRight
     , binaryOp "%" Modulo AssocRight
     ]
   , [ binaryOp "+" Add AssocRight
     , binaryOp "-" Sub AssocRight
     ]
   , [ binaryOp "&&" And AssocRight
     ]
   , [ prefixOp "!" Inverse
     , binaryOp "||" Or AssocRight
     ]
   , [ binaryOp ">" Gt AssocRight
     , binaryOp ">=" GtE AssocRight
     , binaryOp "<" Lt AssocRight
     , binaryOp "<=" LtE AssocRight
     ]
   , [ binaryOp "==" Eq AssocRight
     , binaryOp "/=" Neq AssocRight
     , binaryOp "++" Concat AssocRight
     ]
   ]

operatorTerm :: Parser Expression
operatorTerm = do
   whiteSpace
   term <- parens parseOperators
         <|> try parseFunctionCall
         <|> try parseIdentifier
         <|> try parseBool
         <|> try parseFloat
         <|> try parseInt
         <|> try parseChar
         <|> try parseString
         <|> try parseList
   whiteSpace
   return term

parseOperators = buildExpressionParser operatorsTable operatorTerm

parseExpression :: Parser Expression
parseExpression =
   try parseFunction
   <|> try parseOperators
   <|> try parseIfElse
   <|> try parseIf
   <|> try parseFunctionCall
   <|> try parseBlock
   <|> try parseIdentifier
   <|> try parseString
   <|> try parseFloat
   <|> try parseInt
   <|> try parseChar
   <|> try parseBool
   <|> try parseList
   <|> try parseLet

programmParser :: Parser Expression
programmParser = do
   programm <- parseExpression
   eof
   return programm
