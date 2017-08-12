module Environment where

import           Types


-- This module contains the `lookup` and `extend` functions
-- which you'll implement during <part 4> of the workshop.


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
