module Parser ( parse, unparse, parseMultiple ) where

import           Data.Char      (isDigit)
import           ParserSolution
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
  --DiySymbol "Implement this function!"
  parse' source


----------------------------------------------------------------
----------------------------------------------------------------


-- The functions below: `parseMultiple` and `unparse` are
-- implemented in order for the REPL to work.
-- Don't worry about them when implementing the language.


-- Parses a string representation of *multiple* expressions
-- and generates the corresponding Abstract Syntax Tree.
parseMultiple :: String -> DiyAST
parseMultiple source =
  case splitExpressions $ removeComments source of
    Left parsingError -> DiyError IncompleteExpression
    Right expressions -> DiyList $ parse <$> expressions


-- Turns a Diy AST back into a string representation.
unparse :: DiyAST -> String
unparse (DiySymbol string)                    = string
unparse (DiyBool True)                        = "#t"
unparse (DiyBool False)                       = "#f"
unparse (DiyInt int)                          = show int
unparse (DiyList (DiySymbol "quote":exps)) = "'" ++ unwords (unparse <$> exps)
unparse (DiyList exps)                        = "(" ++ unwords (unparse <$> exps) ++ ")"
unparse (DiyError IncompleteExpression)        = "Error: Incomplete expression!"
unparse (DiyError ExpressionTooLarge)          = "Error: Expression too large!"
