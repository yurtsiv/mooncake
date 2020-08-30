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
    let vars = [r|
      let x = 1 + 2
      let y = x - 1
      y
    |]

    testProgramm vars (Integer 2)

    -- makign variables negative and positive
    let prefixOps = [r|
      let a = 1 
      let b = 3
      # without outer parens it doesn't parse correctly.
      # Need to think if this is a problem or not
      (+(-a - 3))
    |]

    testProgramm prefixOps (Integer 4)
  
    -- conditions
    let cond1 = [r|
      if True:
        1
    |]

    testProgramm cond1 (Integer 1)

    let cond2 = [r|
      if False:
        1
    |]

    testProgramm cond2 (Empty)

    let cond3 = [r|
      if 0 > 2:
        1
      else:
        0
    |]

    testProgramm cond3 (Integer 0)

    let cond4 = [r|
      if 1 + 1 == 2:
        1
      else:
        0
    |]

    testProgramm cond4 (Integer 1)

    -- functions
    let funcs = [r|
      let a = 10
      let add = (a, b) -> {
        let res = a + b
        res
      }

      add(1, 2) + a
    |]

    testProgramm funcs (Integer 13)

    let fib10 = [r|
      let fib = (n) -> {
        if n < 3:
          1
        else:
          fib(n - 1) + fib(n - 2)
      } 

      fib(10)
    |]

    testProgramm fib10 (Integer 55)

    -- concatenation
    let concat1 = [r|
      [] ++ [] 
    |]

    testProgramm concat1 (List [])

    let concat2 = [r|
      [1] ++ [2] 
    |]

    testProgramm concat2 (List [Integer 1, Integer 2])

    let concat3 = [r|
      [1] ++ ["hello"] 
    |]

    testProgramm concat3 (List [Integer 1, String "hello"])

    let concat4 = [r|
      "hello" ++ " concatenation"
    |]

    testProgramm concat4 (String "hello concatenation")

    let concat5 = [r|
      "hello" ++ [1]
    |]

    testProgramm concat5 (List [String "h", String "e", String "l", String "l", String "o", Integer 1])

    let concat6 = [r|
      [1] ++ "hello"
    |]

    testProgramm concat6 (List [Integer 1, String "h", String "e", String "l", String "l", String "o"])


    -- len function
    let len1 = [r|
      len([]) 
    |]

    testProgramm len1 (Integer 0)

    let len2 = [r|
      len([1, 2, 3]) 
    |]

    testProgramm len2 (Integer 3)

    let len3 = [r|
      len("") 
    |]

    testProgramm len3 (Integer 0)
 
    let len4 = [r|
      len("hello") 
    |]

    testProgramm len4 (Integer 5)

    -- bool operations
    let bool = [r|
      !(True && False || True && True)
    |]

    testProgramm bool (Bool False)