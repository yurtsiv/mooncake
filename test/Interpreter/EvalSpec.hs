{-# LANGUAGE QuasiQuotes #-}
module Interpreter.EvalSpec (spec) where

import Test.Hspec
import Text.RawString.QQ

import Interpreter.Types
import Interpreter.Eval
import qualified Parser.AST as P
import Parser.Parser

testCorrectProgramm :: String -> Result -> Spec
testCorrectProgramm programm expected =
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


testIncorrectProgramm :: String -> Spec
testIncorrectProgramm programm =
  it programm $ do
    case parseProgramm programm of
      Left err -> expectationFailure $ show err
      Right expr -> 
        let e = startEvaluation expr in
        case e of
          Left err -> putStrLn $ show err
          Right res -> expectationFailure $ "Fail! Expected runtime error!"

spec :: Spec
spec = do
  describe "basic correct cases" $ do
    testCorrectProgramm "1" (Integer 1)
    testCorrectProgramm "True" (Bool True)
    testCorrectProgramm "'a'" (Char 'a')
    testCorrectProgramm "\"string\"" (String "string")
    testCorrectProgramm "[1, 2, 3]" (List [Integer 1, Integer 2, Integer 3])
    testCorrectProgramm "1 + 2" (Integer 3)
    testCorrectProgramm "1 - 2" (Integer (-1))
    testCorrectProgramm "6 / 3" (Integer (2))
    testCorrectProgramm "2 * 2" (Integer (4))
    testCorrectProgramm "5 % 2" (Integer (1))

    testCorrectProgramm "1 > 2" (Bool False)
    testCorrectProgramm "2 >= 2" (Bool True)
    testCorrectProgramm "1 < 3" (Bool True)
    testCorrectProgramm "3 <= 3" (Bool True)
    testCorrectProgramm "3 == 2" (Bool False)
    testCorrectProgramm "3 == 3" (Bool True)
    testCorrectProgramm "3 /= 3" (Bool False)
    testCorrectProgramm "3 /= 2" (Bool True)

  describe "more complex cases" $ do
    -- basic variable usage
    let vars = [r|
      let x = 1 + 2
      let y = x - 1
      y
    |]

    testCorrectProgramm vars (Integer 2)

    -- integers
    let integers = [r|
      let a = 1 
      let b = 3
      # without outer parens it doesn't parse correctly.
      # Need to think if this is a problem or not
      (+(-a - 3))
    |]

    testCorrectProgramm integers (Integer 4)

    -- chars
    let chars = [r|
      let a = 'a'
      let b = 'b'

      [
        a == b,
        a /= b,
        a < b,
        a > b,
        a >= b,
        a <= b
      ]
    |]

    testCorrectProgramm chars (List [Bool False, Bool True, Bool True, Bool False, Bool False, Bool True])

    -- floats
    {-let floats = [r|
      let a = -1.2 
      let b = 1.2

      (+(-b + a))
    |]
    -}

    --testCorrectProgramm floats (Float 1.4)

    -- floats and ints
    {-let floats = [r|
      let i = 2
      let f = 2.1

      i + f
    |]
    -}

    --testCorrectProgramm floats (Float 4.1)

    -- conditions
    let cond1 = [r|
      if True then
        1
      end
    |]

    testCorrectProgramm cond1 (Integer 1)

    let cond2 = [r|
      if False then
        1
      end
    |]

    testCorrectProgramm cond2 (Empty)

    let cond3 = [r|
      if 0 > 2 then
        1
      else
        0
      end
    |]

    testCorrectProgramm cond3 (Integer 0)

    let cond4 = [r|
      if 1 + 1 == 2 then
        1
      else
        0
      end
    |]

    testCorrectProgramm cond4 (Integer 1)

    -- functions
    let funcs = [r|
      let a = 10
      let add = (a, b) do
        let res = a + b
        res
      end

      add(1, 2) + a
    |]

    testCorrectProgramm funcs (Integer 13)

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

    testCorrectProgramm fib10 (Integer 55)

    -- concatenation
    let concat1 = [r|
      [] ++ [] 
    |]

    testCorrectProgramm concat1 (List [])

    let concat2 = [r|
      [1] ++ [2] 
    |]

    testCorrectProgramm concat2 (List [Integer 1, Integer 2])

    let concat3 = [r|
      [1] ++ ["hello"] 
    |]

    testCorrectProgramm concat3 (List [Integer 1, String "hello"])

    let concat4 = [r|
      "hello" ++ " concatenation"
    |]

    testCorrectProgramm concat4 (String "hello concatenation")

    let concat5 = [r|
      "hello" ++ [1]
    |]

    testCorrectProgramm concat5 (List [String "h", String "e", String "l", String "l", String "o", Integer 1])

    let concat6 = [r|
      [1] ++ "hello"
    |]

    testCorrectProgramm concat6 (List [Integer 1, String "h", String "e", String "l", String "l", String "o"])


    -- len function
    let len1 = [r|
      len([]) 
    |]

    testCorrectProgramm len1 (Integer 0)

    let len2 = [r|
      len([1, 2, 3]) 
    |]

    testCorrectProgramm len2 (Integer 3)

    let len3 = [r|
      len("") 
    |]

    testCorrectProgramm len3 (Integer 0)
 
    let len4 = [r|
      len("hello") 
    |]

    testCorrectProgramm len4 (Integer 5)

    -- bool operations
    let bool = [r|
      !(True && False || True && True)
    |]

    testCorrectProgramm bool (Bool False)


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

    testCorrectProgramm closure (Integer 21)

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

    testCorrectProgramm hof (Integer 8)

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

    testCorrectProgramm map (List [Integer 2, Integer 3, Integer 4])

    -- test for trying to call non-function or getting a length of non-list
    let badCall = [r|
      let f = 7 
      let val = 2 
      f(val)
    |]

    testIncorrectProgramm badCall

    -- test for using a non-boolean in the if condition
    let badConditionIf = [r|
      if "this is a string" then
        1
      end
    |]

    testIncorrectProgramm badConditionIf

    -- test for using a non-boolean in the if-else condition
    let badConditionIfElse = [r|
      if "this is a string" then
        1
      else
        2
      end
    |]

    testIncorrectProgramm badConditionIfElse

    -- test for using a non-declared variable
    let badVar = [r|
      let f = 7 
      f + g 
    |]

    testIncorrectProgramm badVar

    -- test for using infix '-' on non-integer
    let badInfixM = [r|
      let f = -"string"
    |]

    testIncorrectProgramm badInfixM

    -- test for using infix '+' on non-integer
    let badInfixP = [r|
      let f = +"string"
    |]

    testIncorrectProgramm badInfixP

    -- test for inverting a non-boolean
    let nonBoolInvert = [r|
      let f = 41
      !f
    |]

    testIncorrectProgramm nonBoolInvert

    -- test for invalid concatenation
    let invalidConcat = [r|
      let f = 41
      let g = "my string"
      g ++ f
    |]

    testIncorrectProgramm invalidConcat

    -- test for the '-' operation between non-numbers
    let badAlgebraicMinus = [r|
      let f = 41
      let g = True
      f - g
    |]

    testIncorrectProgramm badAlgebraicMinus

    -- test for the '+' operation between non-numbers
    let badAlgebraicPlus = [r|
      let f = 41
      let g = True
      f + g
    |]

    testIncorrectProgramm badAlgebraicPlus

    -- test for the '*' operation between non-numbers
    let badAlgebraicMul = [r|
      let f = 41
      let g = True
      f * g
    |]

    testIncorrectProgramm badAlgebraicMul

    -- test for the '%' operation between non-numbers
    let badAlgebraicMod = [r|
      let f = 41
      let g = True
      f % g
    |]

    testIncorrectProgramm badAlgebraicMod

    -- test for the '/' operation between non-numbers
    let badAlgebraicDiv = [r|
      let f = 41
      let g = True
      f / g
    |]

    testIncorrectProgramm badAlgebraicDiv

    -- test for dividing by 0
    let zeroDivision = [r|
      let f = 42
      let g = 0 
      f / g
    |]

    testIncorrectProgramm zeroDivision

    -- test for the '&&' operation between non-booleans
    let badBooleanAnd = [r|
      let f = False
      let g = "string"
      f && g
    |]

    testIncorrectProgramm badBooleanAnd

    -- test for the '||' operation between non-booleans
    let badBooleanOr = [r|
      let f = False
      let g = "string"
      f || g
    |]

    testIncorrectProgramm badBooleanOr

    -- test for '==' comparison between non-integers
    let badCompEq = [r|
      let f = "string1"
      let g = "string2"
      f == g
    |]

    testIncorrectProgramm badCompEq

    -- test for '<' comparison between non-integers
    let badCompLt = [r|
      let f = "string1"
      let g = "string2"
      f < g
    |]

    testIncorrectProgramm badCompLt

    -- test for '<=' comparison between non-integers
    let badCompLtE = [r|
      let f = "string1"
      let g = "string2"
      f <= g
    |]

    testIncorrectProgramm badCompLtE

    -- test for '>' comparison between non-integers
    let badCompGt = [r|
      let f = "string1"
      let g = "string2"
      f > g
    |]

    testIncorrectProgramm badCompGt

    -- test for '>=' comparison between non-integers
    let badCompGtE = [r|
      let f = "string1"
      let g = "string2"
      f >= g
    |]

    testIncorrectProgramm badCompGtE

    -- test for getting the length for invalid data type
    let badLen = [r|
      let f = 123
      len(f)
    |]

    testIncorrectProgramm badLen

    -- test for calling the 'len' function with more than 1 argument
    let badLenArgs = [r|
      let f = "str1"
      let g = "str2"
      len(f, g)
    |]

    testIncorrectProgramm badLenArgs

    -- test for calling a user-defined function with invalid number of arguments
    let badFuncArgs = [r|
      let add = (x, y) do
        let sum = x + y
        sum
      end

      let z = add(1, 2, 3)
    |]

    testIncorrectProgramm badFuncArgs

    -- test for accessing list elements with too many arguments
    let longListArgs = [r|
      let l = [10, 20, 30]
      l(1, 2)
    |]

    testIncorrectProgramm longListArgs

    -- test for accessing list elements past the bounds
    let invalidListIndex = [r|
      let l = [10, 20, 30]
      l(3)
    |]

    testIncorrectProgramm invalidListIndex
