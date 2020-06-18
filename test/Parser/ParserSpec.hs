{-# LANGUAGE QuasiQuotes #-}
module Parser.ParserSpec (spec) where
import Data.Either
import Parser.Parser
import System.IO
import Test.Hspec
import Text.RawString.QQ

validProgramm = [r|
let string1 = "hello"
let stringTwo = "new string"
let positiveInt = 123
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
|]

invalidProgramms =
  [ "let"
  , "let val"
  , "let string = \""
  , "let arr = [1 2]"
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
    testCorrectProgramm validProgramm

  describe "invalid progamms" $ do
    mapM_ testIncorrectProgramm invalidProgramms
