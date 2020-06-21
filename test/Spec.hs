module Main where

import Test.Hspec
import qualified Parser.ParserSpec as Parser
import qualified Interpreter.EvalSpec as Interpreter

spec :: Spec
spec = do
  describe "Parser" $ do
    Interpreter.spec
    Parser.spec

main :: IO ()
main = hspec spec
