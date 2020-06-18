{-# LANGUAGE QuasiQuotes #-}

module Parser.ParserSpec (spec) where

import Data.Either
import Parser.Parser
import System.IO
import Test.Hspec
import Text.RawString.QQ

validProgramm = [r|
let stringOne = "hello"
let stringTwo = "new string"
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
