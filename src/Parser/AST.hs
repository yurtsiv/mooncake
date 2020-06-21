module Parser.AST where

data Expression =
  -- Literals
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [Expression]
  | Function [String] Expression

  -- Declaration
  | Let String Expression

  -- Reference to other expression
  | Identifier String

  -- Algebraic operations
  | Add Expression Expression 
  | Sub Expression Expression
  | Div Expression Expression
  | Mul Expression Expression
  | Block [Expression]
  deriving (Show)
