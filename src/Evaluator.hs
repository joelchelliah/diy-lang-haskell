module Evaluator where

import           Environment
import           EvaluatorSolution
import           Parser            (unparse)
import           Prelude                         hiding (lookup)
import           Types


-- This is the Evaluator module. The `evaluate` function below
-- is the heart of your language, and the focus for most of
-- parts <2> through <6>.


----------------------------------------------------------------
----------------------------------------------------------------


-- Evaluates an AST in the specified environment, and produces
-- the AST of the evaluated result along with the updated environment.
evaluate :: DiyAST -> Environment -> (DiyAST, Environment)
evaluate ast env =
  --DiySymbol "Implement this function!"
  evaluate' ast env


----------------------------------------------------------------
----------------------------------------------------------------
