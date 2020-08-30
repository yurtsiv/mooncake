module Interpreter.Eval where

import qualified Data.Map.Strict as Map
import Interpreter.Utils
import qualified Parser.AST as AST

data Result
  = Integer Integer
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
evaluate (AST.Integer i) scope = Right $ (Integer i, scope)
evaluate (AST.String str) scope = Right $ (String str, scope)
evaluate (AST.Bool bool) scope = Right $ (Bool bool, scope)
evaluate (AST.List exprs) scope = do
  res <- sequence $ map (\e -> evaluate e scope) exprs
  return $ (List $ map fst res, scope)
evaluate (AST.Function args expr) scope =
  Right $ (Function args expr, scope)
evaluate e@(AST.FunctionCall name callArgs) scope =
  case Map.lookup name builtInFunctions of
    Just func -> func e scope
    _ -> do
      (res, _) <- evaluate (AST.Identifier name) scope
      case res of
        Function argNames body ->
          if (length callArgs) /= (length argNames)
            then Left $ "Wrong number of arguments provided for " ++ name
            else
              let evaluatedArgs = evaluate (AST.List callArgs) scope
              in case evaluatedArgs of
                    Right (List evalArgs, _) ->
                      let funcScope = mergeScopes scope (Map.fromList $ (zip argNames evalArgs))
                      in evaluate body funcScope
                    Left err -> Left err
        _ -> Left $ name ++ "is not a function"
evaluate (AST.If condition body) scope = do
  (val, _) <- evaluate condition scope
  case val of
    Bool True -> evaluate body scope
    Bool False -> Right $ (Empty, scope)
    _ -> Left "The condition is not a boolean"
evaluate (AST.IfElse condition ifBody elseBody) scope = do
  (val, _) <- evaluate condition scope
  case val of
    Bool True -> evaluate ifBody scope
    Bool False -> evaluate elseBody scope
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
evaluate (AST.Add expr1 expr2) scope = evalAlgebraicOp (+) expr1 expr2 scope
evaluate (AST.Sub expr1 expr2) scope = evalAlgebraicOp (-) expr1 expr2 scope
evaluate (AST.Div expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (Integer v1, Integer v2) ->
      if v2 == 0
        then Left "Can't divide by 0"
        else Right $ (Integer $ v1 `div` v2, scope)
    _ -> Left "Can divide only integers"
evaluate (AST.Mul expr1 expr2) scope = evalAlgebraicOp (*) expr1 expr2 scope
evaluate (AST.Modulo expr1 expr2) scope = evalAlgebraicOp (mod) expr1 expr2 scope
evaluate (AST.Inverse expr) scope = do
  (val, _) <- evaluate expr scope
  case val of
    Bool b -> Right $ (Bool (not b), scope)
    _ -> Left "Can invert only booleans"
evaluate (AST.Or expr1 expr2) scope = evalBinBoolOp (||) expr1 expr2 scope
evaluate (AST.And expr1 expr2) scope = evalBinBoolOp (&&) expr1 expr2 scope
evaluate (AST.Gt expr1 expr2) scope = evalNumCompOp (>) expr1 expr2 scope
evaluate (AST.GtE expr1 expr2) scope = evalNumCompOp (>=) expr1 expr2 scope
evaluate (AST.Lt expr1 expr2) scope = evalNumCompOp (<) expr1 expr2 scope
evaluate (AST.LtE expr1 expr2) scope = evalNumCompOp (<=) expr1 expr2 scope
-- TODO: compare all primitive types
evaluate (AST.Eq expr1 expr2) scope = evalNumCompOp (==) expr1 expr2 scope
evaluate (AST.Block exprs) scope = foldl evalCodeBlockItem (Right (Empty, scope)) exprs
evaluate (AST.Concat expr1 expr2) scope = do
  (val1, _) <- evaluate expr1 scope
  (val2, _) <- evaluate expr2 scope
  case (val1, val2) of
    (List elems1, List elems2) ->
      Right ((List $ elems1 ++ elems2), scope)
    (String str1, String str2) ->
      Right ((String $ str1 ++ str2), scope)
    (String str, List elems) ->
      Right ((List $ (hsStringToMCList str) ++ elems), scope)
    (List elems, String str) ->
      Right ((List $ elems ++ (hsStringToMCList str)), scope)
    _ -> Left "Can't concatenate"

evalCodeBlockItem (Right (_, scope)) expr = evaluate expr scope
evalCodeBlockItem (Left a) _ = Left a

evalAlgebraicOp op expr1 expr2 scope = do
  (res1, _) <- evaluate expr1 scope
  (res2, _) <- evaluate expr2 scope
  case (res1, res2) of
    (Integer val1, Integer val2) ->
      Right $ (Integer $ op val1 val2, scope)
    _ -> Left "Can perform algebraic operation only on numbers"

evalBinBoolOp op expr1 expr2 scope = do
  (res1, _) <- evaluate expr1 scope
  (res2, _) <- evaluate expr2 scope
  case (res1, res2) of
    (Bool b1, Bool b2) -> Right $ (Bool (op b1 b2), scope)
    _ -> Left "Can perform operation only on booleans"

evalNumCompOp op expr1 expr2 scope = do
  (res1, _) <- evaluate expr1 scope
  (res2, _) <- evaluate expr2 scope
  case (res1, res2) of
    (Integer val1, Integer val2) -> Right $ (Bool $ op val1 val2, scope)
    _ -> Left "Can compare only numbers"

flipNumber expr scope errMsg = do
  (val, _) <- evaluate expr scope
  case val of
    Integer i -> Right $ (Integer $ negate i, scope)
    _ -> Left $ errMsg

hsStringToMCList str = map (\c -> String [c]) str

evalLen (AST.FunctionCall _ callArgs) scope =
  case callArgs of
    [arg] -> do
      (val, _) <- evaluate arg scope
      case val of
        List elems -> Right $ (Integer $ (toInteger . length) elems, scope)
        String str -> Right $ (Integer $ (toInteger . length) str, scope)
        _ -> Left "Can't get length"
    _ ->
      Left "Wrong number of arguments provided for function 'len'"

builtInFunctions = Map.fromList [("len", evalLen)]