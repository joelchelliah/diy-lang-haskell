module InterpreterUtil (parseMultiple, unparse) where

import           Parser     (parse)
import           ParserUtil (removeComments, splitExpressions)
import           Types

-- Utility functions used by the interpreter.
-- The functions below: `parseMultiple` and `unparse` are
-- implemented in order for the interpreter and REPL to work.
--
-- You are not required to make any changes in this file as part of
-- the workshop.


-- Parses a string representation of *multiple* expressions
-- and generates the corresponding Abstract Syntax Tree.
parseMultiple :: String -> [DiyAST]
parseMultiple source =
  case splitExpressions $ clean source of
    Right expressions -> parse <$> expressions
    Left parsingError -> []

  where clean = trim . removeComments
        trim  = unwords . concatMap words . filter ((0 <) . length) . lines


-- Turns a Diy AST back into a string representation.
unparse :: DiyAST -> String
unparse (DiySymbol string)                    = string
unparse (DiyBool True)                        = "#t"
unparse (DiyBool False)                       = "#f"
unparse (DiyInt int)                          = show int
unparse (DiyList (DiySymbol "quote":exps)) = "'" ++ unwords (unparse <$> exps)
unparse (DiyList exps)                        = "(" ++ unwords (unparse <$> exps) ++ ")"
unparse (DiyError err)                        = "Error: " ++ show err
unparse other                                 = "Error: Tried to unparse <<" ++ show other ++ ">>"
