{-# LANGUAGE QuasiQuotes #-}
module Parser.ParserSpec (spec) where

import Data.Either
import Parser.Parser
import System.IO
import Test.Hspec
import Text.RawString.QQ

compoundValidProgramm = [r|

let string1 = "hello"
let str1ngTwo = "new string"
let PositiveInt = 123
let intCopy = positiveInt # comment here no problems

# multiline 
# comment

  # comments ignore
 # indentation

let negativeInt = -123
let negation = -positiveInt
let positivationXD = +negation
let booleanT = True
let booleanF = False
let emptyList = []
let oneItemList = [ 1 ]
let multipleItemsList = [1, 2, 3]

let weirdMultiDimList = [
  [1, 2,   
   3, ],
  [4, 5, 6],
]

let listOfBlocks = [
  { let a = 1 a},
  { let b = PositiveInt b }
]

# comment at the end of the file

let function = (a, b) -> {
  let c = a
  c
}

let function = (a, b) -> {a}

let function = (a, b) -> { # not tab sensitive
let c = a
c
}

let continue = 1

let justBlock = {
  let a = 1
  let b = 2 
  a
}
|]

readmeExamples = [r|
let number = 1

let boolean = True

let string = "Hello there!"

let function = (a, b) -> a + b

# lists are heterogenous i.e. can contain elements of different types. It's a comment btw
let list = [number, boolean, string, function] 

# Algebra
let sum = 4 + 2
let sub = 4 - 2
let div = 4 / 2
let mul = 4 * 2
let mod = 4 % 2

# Comparison
let eq = 2 == 2
let gt = 4 > 2
let gte = 4 >= 2
let lt = 2 < 4
let lte = 2 <= 4

# Boolean logic (not fully implemented yet)
let false = !True

# Lists/strings
let listLen = len([1, 2, 3])
let strLen = len("Mooncake")
let listConcat = [1, 2, 3] ++ ["Ready", "or", "not", "here", "I", "come"]
let strConcat = "Hello" ++ " there"
let strListConcat = "Hi" ++ [1, 2, 3] # will result in ["H", "i", 1, 2, 3]
let listStrConcat = [1, 2, 3] ++ "Hi" # will result in [1, 2, 3, "H", "i"]

# Conditional expressions are expressions, not statements i.e. they return something
let truth1 =
  if 4 >= 2:
    "4 is greater or equal than 2"
  else:
    "4 is smaller than 2"

# No elseif yet
let truth2 =
  if !(2 == 2):
    "2 is not equal to 2"
  else: {
    if 3 == 2:
      "3 is equal to 2"
    else:
      "3 is no equal to 2"
  }

# The last statement of a function body is what this function will return
let add = (x, y) -> {
  let sum = x + y
  sum
}

let three = add(1, 2)

# Recursion
let fib = (n) -> {
  if n < 3:
    1
  else:
    fib(n - 1) + fib(n - 2)
}

# Higher order functions
let mul = (x) -> (y) -> x * y
let apply = (f, val) -> f(val)

let eight = apply(mul(4), 2)
|]

validProgramms =
  [ compoundValidProgramm
  , readmeExamples
  , "let a = - 1"
  , "let e = 1 + 2 * x"
  , "let b = !(1 > a)"
  , "let b = \"hello\" == 1"
  , "+ 1"
  , "\"1\" + \"2\""
  , "[1, 2] + [3, 4]"
  , "[(a) -> (a+1)] + [() -> {}]"
  , "myFunc(a) + 2"
  -- This parses as a function call to "a". Not sure it's desired behavior
  , [r|
      let a = 1 
      let b = 3
      let negativeA = -a

      (+(negativeA - 3))
    |]
  , [r|
      if True:
        False

      if (1 + 2) == 3:
        "yay, correct"
      else: {
        if 3 + 5 == 8:
          "Horray"
      }
    |]
  , "[] ++ []"
  , "\"string1\" ++ []"
  , "func(a) ++ func(b)"
  , "len([])"
  ]
  
invalidProgramms =
  [ "let"
  , "let val"

  -- Missing closing quote
  , "let string = \""

  -- Missing comma
  , "let arr = [1 2]"

  -- Not allowed identifiers
  , "let _noSpecSymbolsAtStart = 1"
  , "let 1noNumsAtStart = 1"
  , "let no_snake_case = 1"
  , "let let = 1"
  , "let True = False"
  , "let False = True"
  , "let if = False"
  , "let len = 2"
  ]


testIncorrectProgramm :: String -> Spec
testIncorrectProgramm programm =
  it programm $ do
    case parseProgramm programm of
      Left err -> putStrLn $ show err
      Right val -> expectationFailure $ show val

testCorrectProgramm :: String -> Spec
testCorrectProgramm programm =
  it programm $ do
    case parseProgramm programm of
      Left err -> expectationFailure $ show err
      Right val -> putStrLn $ show val

spec :: Spec
spec = do
  describe "valid programm" $ do
    mapM_ testCorrectProgramm validProgramms

  describe "invalid progamms" $ do
    mapM_ testIncorrectProgramm invalidProgramms
