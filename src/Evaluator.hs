module Evaluator where

import           Parser (unparse)
import           Types


-- This is the Evaluator module. The `evaluate` function below
-- is the heart of your language, and the focus for most of
-- parts <2> through <6>.


----------------------------------------------------------------
----------------------------------------------------------------


-- Evaluates an AST in the specified environment.
evaluate :: Parsed -> String -> Parsed
evaluate ast env =
  ParsedSymbol "Implement this function!"


----------------------------------------------------------------
----------------------------------------------------------------
