module Main where

import Parser.Parser (readProgramm)
import System.Environment

main :: IO ()
main = do args <- getArgs
          putStrLn (readProgramm $ args !! 0)