module Environment where

import           Types

-- This module holds the `Environment` type which will be used
-- to represent key-value bindings that are stored, and looked up,
-- by our language during the evaluation phase.
-- It contains the `lookup` and `extend` functions which you'll
-- implement during <part 4> of the workshop.


-- Represents the environment, holding a list of all
-- (key, value) bindings created during an evaluation.
newtype Environment = Environment [(String, DiyAST)] deriving (Show)


----------------------------------------------------------------
----------------------------------------------------------------


-- Looks up a <DiyAST> value in the environment that
-- is bound to the given `key`.
lookup :: Environment -> String -> DiyAST
lookup env key =
  DiySymbol "Implement this function!"


-- Extends the environment with an additional
-- (key, value) binding.
extend :: Environment -> (String, DiyAST) -> Environment
extend env (key, val) =
  Environment [("", DiySymbol "Implement this function!")]


----------------------------------------------------------------
----------------------------------------------------------------
