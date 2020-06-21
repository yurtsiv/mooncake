module Parser.AST where

data Expression =
  -- Literals
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [Expression]
  | Function [String] Programm

  -- Reference to other expression
  | Identifier String

  -- Algebraic operations
  | Add Expression Expression 
  | Sub Expression Expression
  | Div Expression Expression
  | Mul Expression Expression
  deriving (Show)

data Component =
    Expression Expression
  | Declaration String Expression
  | Noop
  deriving (Show)

type Programm = [Component]