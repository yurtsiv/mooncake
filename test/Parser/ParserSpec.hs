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

validProgramms =
  [ compoundValidProgramm
  , "let a = - 1"
  , "let e = 1 + 2 * x"
  , "let b = !(1 > a)"
  , "let b = \"hello\" == 1"
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
