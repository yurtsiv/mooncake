module Interpreter.Eval where

import qualified Parser.AST as P

data Result =
    Integer Integer
  | String String
  | Bool Bool
  | List [Result]
  deriving (Eq, Ord, Show)

evaluate :: P.Expression -> Either String Result

evaluate (P.Integer int) = Right $ Integer int
evaluate (P.String s) = Right $ String s
evaluate (P.Bool b) = Right $ Bool b
evaluate (P.List exprs) = do
  res <- sequence $ map evaluate exprs
  return $ List res


evaluate (P.Function args expr) = Left "not implemented"
evaluate (P.Let name expr) = Left "not implemented"
evaluate (P.Identifier name) = Left "not implemented"

evaluate (P.Add expr1 expr2) = do
  val1 <- evaluate expr1
  val2 <- evaluate expr2
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ Integer $ v1 + v2
    _ -> Left "Can add only integers"

evaluate (P.Sub expr1 expr2) = do
  val1 <- evaluate expr1
  val2 <- evaluate expr2
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ Integer $ v1 - v2
    _ -> Left "Can subtract only integers"

evaluate (P.Div expr1 expr2) = do
  val1 <- evaluate expr1
  val2 <- evaluate expr2
  case (val1, val2) of
    (Integer v1, Integer v2) ->
      if v2 == 0 then
        Left "Can't divide by 0"
      else
        Right $ Integer $ v1 `div` v2
    _ -> Left "Can divide only integers"

evaluate (P.Mul expr1 expr2) = do
  val1 <- evaluate expr1
  val2 <- evaluate expr2
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ Integer $ v1 * v2
    _ -> Left "Can multiply only integers"

evaluate (P.Modulo expr1 expr2) = do
  val1 <- evaluate expr1
  val2 <- evaluate expr2
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ Integer $ v1 `mod` v2
    _ -> Left "Can perform modulo only on integers"

evaluate (P.Inverse expr) = do
  val <- evaluate expr
  case val of
    Bool b -> Right $ Bool (not b)
    _ -> Left "Trying to invert non boolean"
  
evaluate (P.Gt expr1 expr2) = evalCompOp (>) expr1 expr2
evaluate (P.GtE expr1 expr2) = evalCompOp (>=) expr1 expr2
evaluate (P.Lt expr1 expr2) = evalCompOp (<) expr1 expr2
evaluate (P.LtE expr1 expr2) = evalCompOp (<=) expr1 expr2
evaluate (P.Eq expr1 expr2) = evalCompOp (==) expr1 expr2

evaluate (P.Block exprs) =
  last evaluated
  where evaluated = map evaluate exprs

evalCompOp op expr1 expr2 = do
  Right $ Bool $ op (evaluate expr1) (evaluate expr2)
