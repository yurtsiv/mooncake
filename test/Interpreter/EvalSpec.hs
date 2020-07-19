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
        let e = startEvaluation expr in
        case e of
          Right res ->
            case (res == expected) of
              True -> putStrLn "Success"
              False -> expectationFailure $ "Fail! Expected " ++ (show expected) ++ " and got " ++ (show res)
          Left err -> expectationFailure $ "Fail! " ++ (show err) ++ ". AST " ++ (show expr)

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

  describe "more complex cases" $ do
    -- basic variable usage
    let p1 = [r|
      let x = 1 + 2
      let y = x - 1
      y
    |]

    testProgramm p1 (Integer 2)

    -- makign variables negative and positive
    let p2 = [r|
      let a = 1 
      let b = 3
      # without outer parens it doesn't parse correctly.
      # Need to think if this is a problem or not
      (+(-a - 3))
    |]

    testProgramm p2 (Integer 4)

    -- functions
    let p3 = [r|
      let a = 10
      let add = (a, b) -> {
        let res = a + b
        res
      }

      add(1, 2) + a
    |]

    testProgramm p3 (Integer 13)
