module Interpreter.Eval where

import Data.Map.Merge.Strict
import qualified Data.Map.Strict as M
import qualified Parser.AST as P

data Result =
    Integer Integer
  | String String
  | Bool Bool
  | List [Result]
  | Function [String] P.Expression
  | Empty
  deriving (Eq, Ord, Show)

type Scope = M.Map String Result

startEvaluation :: P.Expression -> Either String Result
startEvaluation expr = do
  (res, _) <- evaluate expr M.empty
  return res

evaluate :: P.Expression -> Scope -> Either String (Result, Scope)

evaluate (P.Integer int) scope = Right $ (Integer int, scope)
evaluate (P.String s) scope = Right $ (String s, scope)
evaluate (P.Bool b) scope = Right $ (Bool b, scope)
evaluate (P.List exprs) scope = do
  res <- sequence $ map (\e -> evaluate e scope) exprs
  return $ (List $ map fst res, scope)

evaluate (P.Function args expr) s =
  Right $ (Function args expr, s)

evaluate (P.FunctionCall name callArgs) s =
  case evaluate (P.Identifier name) s of
    Right (Function argNames body, s) ->
      if (length callArgs) /= (length argNames) then
        Left $ "Wrong number of arguments provided for " ++ name
      else
        let evalArgsRes = evaluate (P.List callArgs) s
        in case evalArgsRes of
          Right (List evalArgs, _) ->
            let funcScope = merge preserveMissing
                                  preserveMissing
                                  (zipWithMatched (\_ -> \_ -> \x -> x))
                                  s
                                  (M.fromList $ (zip argNames evalArgs))
            in evaluate body funcScope
          Left err -> Left err
    Right _ -> Left (name ++ " is not a function")
    Left err -> Left err

evaluate (P.Let name expr) scope = do
  (val, _) <- evaluate expr scope
  return (Empty, M.insert name val scope)

evaluate (P.Identifier name) scope =
  case (M.lookup name scope) of
    Just val -> Right (val, scope)
    Nothing -> Left $ "No variable named " ++ name

evaluate (P.Negative expr) scope = flipNumber expr scope "Infix '-' can be applied only to integers"
evaluate (P.Positive expr) scope = flipNumber expr scope "Infix '+' can be applied only to integers"

evaluate (P.Add expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 + v2, scope)
    _ -> Left "Can add only integers"

evaluate (P.Sub expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 - v2, scope)
    _ -> Left "Can subtract only integers"

evaluate (P.Div expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) ->
      if v2 == 0 then
        Left "Can't divide by 0"
      else
        Right $ (Integer $ v1 `div` v2, scope)
    _ -> Left "Can divide only integers"

evaluate (P.Mul expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 * v2, scope)
    _ -> Left "Can multiply only integers"

evaluate (P.Modulo expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) -> Right $ (Integer $ v1 `mod` v2, scope)
    _ -> Left "Can perform modulo only on integers"

evaluate (P.Inverse expr) scope = do
  (val, _) <- evaluate expr scope
  case val of
    Bool b -> Right $ (Bool (not b), scope)
    _ -> Left "Trying to invert non boolean"
  
evaluate (P.Gt expr1 expr2) s = evalCompOp (>) expr1 expr2 s
evaluate (P.GtE expr1 expr2) s = evalCompOp (>=) expr1 expr2 s
evaluate (P.Lt expr1 expr2) s = evalCompOp (<) expr1 expr2 s
evaluate (P.LtE expr1 expr2) s = evalCompOp (<=) expr1 expr2 s
evaluate (P.Eq expr1 expr2) s = evalCompOp (==) expr1 expr2 s

evaluate (P.Block exprs) scope = do
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
