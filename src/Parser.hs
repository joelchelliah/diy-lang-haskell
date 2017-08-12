module Parser ( parse ) where

import           Data.Char  (isDigit)
import           ParserUtil
import           Types


-- This is the Parser module, with the `parse` function which you'll
-- implement as <part 1> of the workshop. Its job is to convert strings
-- into data structures that the evaluator can understand.
--
-- Tips:
--  * A few helpful utility functions, to help you along the
--    way, can be found in `util/ParserUtil.hs`.
--
--  * The types we will be using to represent our AST can be
--    found in `src/Types.hs`.


----------------------------------------------------------------
----------------------------------------------------------------


-- Parses the string representation of a *single* expression and
-- generates the corresponding Abstract Syntax Tree (AST).
parse :: String -> DiyAST
parse source =
  DiySymbol "Implement this function!"


----------------------------------------------------------------
----------------------------------------------------------------
