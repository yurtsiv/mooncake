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
  deriving (Show)

data Component =
    Literal
  | VarDeclaration String Reference
  | FuncDeclaration String Component
  | Noop
  deriving (Show)

type Programm = [Component]