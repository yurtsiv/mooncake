{-# LANGUAGE QuasiQuotes #-}
module Interpreter.EvalSpec (spec) where

import Test.Hspec
import Text.RawString.QQ

import Interpreter.Eval
import qualified Parser.AST as P
import Parser.Parser

testProgramm :: String -> Result -> Spec
testProgramm programm expected =
  it programm $ do
    case parseProgramm programm of
      Left err -> expectationFailure $ show err
      Right expr -> 
        let e = evaluate expr in
        case (e == expected) of
          True -> putStrLn "Success"
          False -> expectationFailure "Fail"

spec :: Spec
spec = do
  describe "basic correct cases" $ do
    testProgramm "1" (Integer 1)
    testProgramm "True" (Bool True)
    testProgramm "\"string\"" (String "string")
    testProgramm "[1, 2, 3]" (List [Integer 1, Integer 2, Integer 3])
    testProgramm "1 + 2" (Integer 3)
    testProgramm "1 - 2" (Integer (-1))
    testProgramm "6 / 3" (Integer (2))
    testProgramm "2 * 2" (Integer (4))
    testProgramm "5 % 2" (Integer (1))

    testProgramm "1 > 2" (Bool False)
    testProgramm "2 >= 2" (Bool True)
    testProgramm "1 < 3" (Bool True)
    testProgramm "3 <= 3" (Bool True)
    testProgramm "3 == 2" (Bool False)

  -- describe "more complex cases" $ do
  --   let p1 = [r|
  --     let x = 1 + 2
  --     let y = x - 1
  --     y
  --   |]

    -- testProgramm p1 (Integer 2)
