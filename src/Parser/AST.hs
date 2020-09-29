module Parser.AST where

data Expression =
  -- Literals
    Integer Integer
  | Float Double
  | String String
  | Bool Bool
  | List [Expression]

  -- Functions
  | Function [String] Expression
  | FunctionCall String [Expression]
  
  -- Conditionals
  | If Expression Expression
  | IfElse Expression Expression Expression

  -- Variable declaration
  | Let String Expression

  -- Reference to other expression
  | Identifier String

  -- Algebraic operations
  | Negative Expression
  | Positive Expression
  | Add Expression Expression 
  | Sub Expression Expression
  | Div Expression Expression
  | Mul Expression Expression
  | Modulo Expression Expression

  -- Boolean operators
  | Inverse Expression
  | And Expression Expression
  | Or Expression Expression

  -- Comparison operations
  | Gt Expression Expression
  | GtE Expression Expression
  | Lt Expression Expression
  | LtE Expression Expression
  | Eq Expression Expression
  | NotEq Expression Expression

  -- List/string operations
  | Concat Expression Expression

  -- A group of expressions
  | Block [Expression]
  deriving (Eq, Ord, Show)
