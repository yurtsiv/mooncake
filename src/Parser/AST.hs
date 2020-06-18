module Parser.AST where

data ListItem =
    Literal MCValue
  | Identifier String
  deriving (Show)

data MCValue =
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [ListItem]
  | Tuple [MCValue]
  deriving (Show)

data AST =
    MCValue
  | ValDeclaration String MCValue
  | FunctionDeclaration String AST
  | Programm [AST]
  deriving (Show)