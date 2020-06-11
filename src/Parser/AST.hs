module Parser.AST (
  MCValue(..)
, AST(..)
) where

data MCValue =
    Float Double
  | Integer Integer
  | String String
  | Bool Bool
  | List [MCValue]
  | Tuple [MCValue]
  deriving (Show)

data AST =
    MCValue
  | ValueDeclaration String MCValue
  | FunctionDeclaration String AST
  deriving (Show)