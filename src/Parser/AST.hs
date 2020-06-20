module Parser.AST where

data Reference =
    Literal MCLiteral
  | Identifier String
  deriving (Show)

data MCLiteral =
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [Reference]
  | Tuple [MCLiteral]
  deriving (Show)

data Component =
    MCLiteral
  | VarDeclaration String Reference
  | FuncDeclaration String Component
  | Noop
  deriving (Show)

type Programm = [Component]