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
let integer = 123
let booleanT = True
let booleanF = False
let regularList = [1, 2, 3]
let multilineList = [
  "s1",
  "s2","s3", "s4"
  string1
]
let multiDimList = [
  [1, 2, 3],
  [1, 3, 4]
]
|]

invalidProgramms =
  [ "let"
  , "let val"
  , [r|let string = "|]
  ]


testIncorrectProgramm :: String -> Spec
testIncorrectProgramm programm =
  it programm $ do
    case parseProgramm programm of
      Left err ->  True
      Right val -> False

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
