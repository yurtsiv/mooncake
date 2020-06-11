module Main where

import Test.Hspec
import qualified Parser.ParserSpec as Parser

spec :: Spec
spec = do
  describe "Parser" Parser.spec

main :: IO ()
main = hspec spec
