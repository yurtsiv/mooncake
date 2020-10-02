module Interpreter.Types where

import qualified Data.Map.Strict as Map
import qualified Parser.AST as AST

type Scope = Map.Map String Result

data Result
  = Integer Integer
  | Float Double
  | Char Char
  | String String
  | Bool Bool
  | List [Result]
  | Function Scope [String] AST.Expression
  | Empty
  deriving (Eq, Ord, Show)