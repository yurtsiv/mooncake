module Interpreter.Utils where

import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict
import Interpreter.Types

-- s2 overrides s1 (i.e. variable shadowing)
mergeScopes :: [Scope] -> Scope
mergeScopes =
  foldl
    (merge
      preserveMissing
      preserveMissing
      (zipWithMatched (\_ -> \_ -> \x -> x)))
    Map.empty
    
