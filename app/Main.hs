module Main where

import Parser.Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let text = args !! 0
  putStrLn $
    case parseProgramm text of
      Left err -> "Error: " ++ show err
      Right val -> show val
