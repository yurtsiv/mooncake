module Parser.AST where

data Reference =
    Literal MCValue
  | Identifier String
  deriving (Show)

data MCValue =
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [Reference]
  | Tuple [MCValue]
  deriving (Show)

data AST =
    MCValue
  | ValDeclaration String Reference
  | FunctionDeclaration String AST
  | Programm [AST]
  deriving (Show)