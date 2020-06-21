module Interpreter.Eval where

import qualified Parser.AST as P

data Result =
    Integer Integer
  | String String
  | Bool Bool
  | List [Result]
  | Error String
  deriving (Eq, Ord, Show)

evaluate :: P.Expression -> Result

evaluate (P.Integer int) = Integer int
evaluate (P.String s) = String s
evaluate (P.Bool b) = Bool b
evaluate (P.List exprs) =
  List $ map evaluate exprs

evaluate (P.Function args expr) = Error "not implemented"
evaluate (P.Let name expr) = Error "not implemented"
evaluate (P.Identifier name) = Error "not implemented"

evaluate (P.Add expr1 expr2) = evalAlgOp (+) expr1 expr2
evaluate (P.Sub expr1 expr2) = evalAlgOp (-) expr1 expr2
evaluate (P.Div expr1 expr2) = evalAlgOp (div) expr1 expr2
evaluate (P.Mul expr1 expr2) = evalAlgOp (*) expr1 expr2
evaluate (P.Modulo expr1 expr2) = evalAlgOp (mod) expr1 expr2

evaluate (P.Inverse expr) =
  case (evaluate expr) of
    Bool b -> Bool (not b)
    _ -> Error "Trying to invert non boolean"
  
evaluate (P.Gt expr1 expr2) = evalCompOp (>) expr1 expr2
evaluate (P.GtE expr1 expr2) = evalCompOp (>=) expr1 expr2
evaluate (P.Lt expr1 expr2) = evalCompOp (<) expr1 expr2
evaluate (P.LtE expr1 expr2) = evalCompOp (<=) expr1 expr2
evaluate (P.Eq expr1 expr2) = evalCompOp (==) expr1 expr2

evaluate (P.Block exprs) =
  last evaluated
  where evaluated = map evaluate exprs

evalAlgOp op expr1 expr2 =
  case (evaluate expr1) of
    Integer i1 ->
      case (evaluate expr2) of
        Integer i2 -> Integer (op i1 i2)
        _ -> Error "Can add only integers"
    _ -> Error "Can add only integers"

evalCompOp op expr1 expr2 =
  Bool $ op (evaluate expr1) (evaluate expr2)
  