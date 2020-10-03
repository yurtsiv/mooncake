{-# LANGUAGE QuasiQuotes #-}
module Interpreter.EvalSpec (spec) where

import Test.Hspec
import Text.RawString.QQ

import Interpreter.Types
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

    -- integers
    let integers = [r|
      let a = 1 
      let b = 3
      # without outer parens it doesn't parse correctly.
      # Need to think if this is a problem or not
      (+(-a - 3))
    |]

    testProgramm integers (Integer 4)

    -- floats
    {-let floats = [r|
      let a = -1.2 
      let b = 1.2

      (+(-b + a))
    |]
    -}

    --testProgramm floats (Float 1.4)

    -- floats and ints
    {-let floats = [r|
      let i = 2
      let f = 2.1

      i + f
    |]
    -}

    --testProgramm floats (Float 4.1)

    -- conditions
    let cond1 = [r|
      if True then
        1
      end
    |]

    testProgramm cond1 (Integer 1)

    let cond2 = [r|
      if False then
        1
      end
    |]

    testProgramm cond2 (Empty)

    let cond3 = [r|
      if 0 > 2 then
        1
      else
        0
      end
    |]

    testProgramm cond3 (Integer 0)

    let cond4 = [r|
      if 1 + 1 == 2 then
        1
      else
        0
      end
    |]

    testProgramm cond4 (Integer 1)

    -- functions
    let funcs = [r|
      let a = 10
      let add = (a, b) do
        let res = a + b
        res
      end

      add(1, 2) + a
    |]

    testProgramm funcs (Integer 13)

    let fib10 = [r|
      let fib = (n) do
        if n < 3 then
          1
        else
          fib(n - 1) + fib(n - 2)
        end
      end

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


    -- closure & shadowing
    let closure = [r|
      let a = 1
      let b = 2
      let c = 3

      let f1 = (a) do
        let b = 4
        let d = 5

        let f2 = () do
          # a = 6, b = 4, c = 3, d = 5
          a + b + c + d
        end

        f2
      end

      let f2 = f1(6)

      # a = 1, b = 2
      f2() + a + b
    |]

    testProgramm closure (Integer 21)

    -- higher order functions
    let hof = [r|
      let mul = (x) do
        (y) do x * y end
      end
      let apply = (f, val) do
        f(val)
      end

      apply(mul(4), 2)
    |]

    testProgramm hof (Integer 8)

    let map = [r|
      let mapHelp = (list, func, index) do
        if (index == len(list)) then
          []
        else
          [func(list(index))] ++ mapHelp(list, func, index + 1)
        end
      end

      let map = (list, func) do mapHelp(list, func, 0) end

      let add1 = (x) do x + 1 end

      map([1,2,3], add1)
    |]

    testProgramm map (List [Integer 2, Integer 3, Integer 4])
