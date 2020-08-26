module Interpreter.Utils where

import Data.Map.Merge.Strict
import Data.Map.Strict

-- s2 overrides s1 (i.e. variable shadowing)
mergeScopes s1 s2 =
  merge
    preserveMissing
    preserveMissing
    (zipWithMatched (\_ -> \_ -> \x -> x))
    s1
    s2
