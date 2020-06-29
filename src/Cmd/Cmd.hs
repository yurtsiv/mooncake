module Cmd.Cmd where

import System.IO
import Control.Monad
import Interpreter.Eval
import Parser.Parser
import Parser.AST

parseProgrammIO :: String -> IO Expression
parseProgrammIO programm =
  case parseProgramm programm of
    Left err -> fail $ show err
    Right expr -> return expr

evalProgrammIO :: Expression -> IO String
evalProgrammIO expr =
  case startEvaluation expr of
    Right res -> return $ show res
    Left err -> fail $ show err

evalFile :: String -> IO String 
evalFile filePath = do
  handle <- openFile filePath ReadMode
  content <- hGetContents handle
  expr <- parseProgrammIO content
  evalProgrammIO expr