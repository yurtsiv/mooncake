{-# LANGUAGE QuasiQuotes #-}

module Parser.ParserSpec (spec) where

import Data.Either
import Parser.Parser
import System.IO
import Test.Hspec
import Text.RawString.QQ

validProgramm = [r|let string = "hello"|]

invalidProgramms =
  [ "let"
  , "let val"
  , [r|let string = "|]
  ]



testIncorrectProgramm :: String -> Spec
testIncorrectProgramm programm =
  it programm $ do
    case parseProgramm programm of
      Left err ->  True `shouldBe` True 
      Right val -> expectationFailure $ show val

testCorrectProgramm :: String -> Spec
testCorrectProgramm programm =
  it programm $ do
    case parseProgramm programm of
      Left err -> expectationFailure $ show err
      Right val -> True `shouldBe` True 

spec :: Spec
spec = do
  describe "valid programms" $ do
    testCorrectProgramm validProgramm

  describe "invalid progamms" $ do
    mapM_ testIncorrectProgramm invalidProgramms
