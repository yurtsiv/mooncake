module Interpreter.Eval where

import Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
import qualified Parser.AST as AST

data Result =
    Integer Integer
  | String String
  | Bool Bool
  | List [Result]
  | Function [String] AST.Expression
  | Empty
  deriving (Eq, Ord, Show)

type Scope = Map.Map String Result

startEvaluation :: AST.Expression -> Either String Result
startEvaluation expr = do
  (res, _) <- evaluate expr Map.empty
  return res

evaluate :: AST.Expression -> Scope -> Either String (Result, Scope)

evaluate (AST.Integer int) scope = Right $ (Integer int, scope)
evaluate (AST.String s) scope = Right $ (String s, scope)
evaluate (AST.Bool b) scope = Right $ (Bool b, scope)
evaluate (AST.List exprs) scope = do
  res <- sequence $ map (\e -> evaluate e scope) exprs
  return $ (List $ map fst res, scope)

evaluate (AST.Function args expr) s =
  Right $ (Function args expr, s)

evaluate (AST.FunctionCall name callArgs) s =
  case evaluate (AST.Identifier name) s of
    Right (Function argNames body, s) ->
      if (length callArgs) /= (length argNames) then
        Left $ "Wrong number of arguments provided for " ++ name
      else
        let evalArgsRes = evaluate (AST.List callArgs) s
        in case evalArgsRes of
          Right (List evalArgs, _) ->
            let funcScope = merge preserveMissing
                                  preserveMissing
                                  (zipWithMatched (\_ -> \_ -> \x -> x))
                                  s
                                  (Map.fromList $ (zip argNames evalArgs))
            in evaluate body funcScope
          Left err -> Left err
    Right _ -> Left (name ++ " is not a function")
    Left err -> Left err

evaluate (AST.If condition body) s = do
  (val, _) <- evaluate condition s
  case val of
    Bool True -> evaluate body s
    Bool False -> Right $ (Empty, s)
    _ -> Left "The condition is not a boolean"

evaluate (AST.IfElse condition ifBody elseBody) s = do
  (val, _) <- evaluate condition s
  case val of
    Bool True -> evaluate ifBody s
    Bool False -> evaluate elseBody s
    _ -> Left "The condition is not a boolean"

evaluate (AST.Let name expr) scope = do
  (val, _) <- evaluate expr scope
  return (Empty, Map.insert name val scope)

evaluate (AST.Identifier name) scope =
  case (Map.lookup name scope) of
    Just val -> Right (val, scope)
    Nothing -> Left $ "No variable named " ++ name

evaluate (AST.Negative expr) scope = flipNumber expr scope "Infix '-' can be applied only to integers"
evaluate (AST.Positive expr) scope = flipNumber expr scope "Infix '+' can be applied only to integers"

evaluate (AST.Add expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 + v2, scope)
    _ -> Left "Can add only integers"

evaluate (AST.Sub expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 - v2, scope)
    _ -> Left "Can subtract only integers"

evaluate (AST.Div expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) ->
      if v2 == 0 then
        Left "Can't divide by 0"
      else
        Right $ (Integer $ v1 `div` v2, scope)
    _ -> Left "Can divide only integers"

evaluate (AST.Mul expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 * v2, scope)
    _ -> Left "Can multiply only integers"

evaluate (AST.Modulo expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 `mod` v2, scope)
    _ -> Left "Can perform modulo only on integers"

evaluate (AST.Inverse expr) scope = do
  (val, _) <- evaluate expr scope
  case val of
    Bool b -> Right $ (Bool (not b), scope)
    _ -> Left "Trying to invert non boolean"
  
evaluate (AST.Gt expr1 expr2) s = evalCompOp (>) expr1 expr2 s
evaluate (AST.GtE expr1 expr2) s = evalCompOp (>=) expr1 expr2 s
evaluate (AST.Lt expr1 expr2) s = evalCompOp (<) expr1 expr2 s
evaluate (AST.LtE expr1 expr2) s = evalCompOp (<=) expr1 expr2 s
evaluate (AST.Eq expr1 expr2) s = evalCompOp (==) expr1 expr2 s

evaluate (AST.Block exprs) scope = do
  res <- foldl evalCodeBlockItem (Right (Integer 1, scope)) exprs
  return res

evalCompOp op expr1 expr2 scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  Right $ (Bool $ op val1 val2, scope)

evalCodeBlockItem (Right (_, scope)) expr = evaluate expr scope
evalCodeBlockItem (Left a) _ = Left a

flipNumber expr scope errMsg = do
  (val, _) <- evaluate expr scope
  case val of
    Integer i -> Right $ (Integer $ negate i, scope)
    _ -> Left $ errMsg
