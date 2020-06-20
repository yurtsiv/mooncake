module Parser.AST where

data Reference =
    Value Literal
  | Identifier String
  deriving (Show)

data Literal =
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [Reference]
  | Function [String] Programm
  deriving (Show)

data Component =
    Reference Reference
  | Declaration String Reference
  | Noop
  deriving (Show)

type Programm = [Component]