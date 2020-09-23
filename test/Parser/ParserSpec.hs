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
let float = 1.2

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

# weird names
let let1 = 123
let then1 = 123

let weirdMultiDimList = [
  [1, 2,   
   3, ],
  [4, 5, 6],
]

let listOfBlocks = [
  do let a = 1 a end,
  do let b = PositiveInt b end
]

# comment at the end of the file

let function = (a, b) do
  let c = a
  c
end

let function = (a, b) do a end

let function = (a, b) do # not tab sensitive
let c = a
c
end

let continue = 1

let justBlock = do
  let a = 1
  let b = 2 
  a
end
|]

-- Examples from README
readmeExamples = [r|
let number = 1

let boolean = True

let string = "Hello there!"

let function = (a, b) do
  a + b
end

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
  if 4 >= 2 then
    "4 is greater or equal than 2"
  else
    "4 is smaller than 2"
  end

# No elseif yet
let truth2 =
  if !(2 == 2) then
    "2 is not equal to 2"
  else
    if 3 == 2 then
      "3 is equal to 2"
    else
      "3 is not equal to 2"
    end
  end

# The last statement of a function body is what the function returns
let add = (x, y) do
  let sum = x + y
  sum
end

let three = add(1, 2)

# Recursion
let fib = (n) do
  if n < 3 then
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

# Higher order functions
let mul = (x) do
  (y) do x * y end
end

let apply = (f, val) do
  f(val)
end

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
  , "[(a) do (a+1) end] + [() do end]"
  , "myFunc(a) + 2"
  -- This parses as a function call to "a". Not sure it's desired behavior
  , [r|
      let a = 1 
      let b = 3
      let negativeA = -a

      (+(negativeA - 3))
    |]
  , [r|
      if True then
        False
      end

      if (1 + 2) == 3 then
        "yay, correct"
      else
        if 3 + 5 == 8 then
          "Horray"
        end
      end
    |]
  , "[] ++ []"
  , "\"string1\" ++ []"
  , "func(a) ++ func(b)"
  , "len([])"
  , "let res = True && False || False && True"
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
  , "let float = 1,2"
  , "let float = 1. 2"
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
