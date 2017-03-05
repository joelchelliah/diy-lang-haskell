module Parser ( Parsed(..)
              , ParseErrorType(..)
              , parse, unparse, parseMultiple
              ) where

import           Data.Char  (isDigit)
import           ParserUtil

-- This is the Parser module, with the `parse` function which you'll
-- implement as <part 1> of the workshop. Its job is to convert strings
-- into data structures that the evaluator can understand.


-- The types we will be using to represent the different parts
-- of our AST. You shouldn't need to define any other types.
data Parsed = ParsedSymbol String
            | ParsedBool Bool
            | ParsedInt Int
            | ParsedList [Parsed]
            | ParsedQuote Parsed
            | ParseError ParseErrorType
            deriving (Show, Eq)

-- Possible parser errors types.
data ParseErrorType = IncompleteExpression
                    | ExpressionTooLarge
                    deriving (Show, Eq)


----------------------------------------------------------------
----------------------------------------------------------------


-- Parses the string representation of a *single* expression and
-- generates the corresponding Abstract Syntax Tree (AST).
parse :: String -> Parsed
parse source =
  ParsedSymbol "Implement this function!"

  -- Tip: A few userful utility functions, to help you along the
  --      way, can be found in `util/ParserUtil.hs`.




----------------------------------------------------------------
----------------------------------------------------------------


-- The functions below: `parseMultiple` and `unparse` are
-- implemented in order for the REPL to work.
-- Don't worry about them when implementing the language.


-- Parses a string representation of *multiple* expressions
-- and generates the corresponding Abstract Syntax Tree.
parseMultiple :: String -> Parsed
parseMultiple source =
  case splitExpressions $ removeComments source of
    Left parsingError -> ParseError IncompleteExpression
    Right expressions -> ParsedList $ parse <$> expressions


-- Turns a Parsed AST back into a string representation.
unparse :: Parsed -> String
unparse (ParsedSymbol string)             = string
unparse (ParsedBool True)                 = "#t"
unparse (ParsedBool False)                = "#f"
unparse (ParsedInt int)                   = show int
unparse (ParsedList exps)                 = "(" ++ unwords (unparse <$> exps) ++ ")"
unparse (ParsedQuote exp)                 = "'" ++ unparse exp
unparse (ParseError IncompleteExpression) = "Error: Incomplete expression!"
unparse (ParseError ExpressionTooLarge)   = "Error: Expression too large!"
