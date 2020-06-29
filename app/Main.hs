module Main where

import Parser.Parser
import System.Environment

import Cmd.Cmd

main :: IO ()
main = do
  args <- getArgs
  let filePath = args !! 0
  res <- evalFile filePath
  putStrLn res
