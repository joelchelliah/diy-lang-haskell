module Parser ( Parsed(..)
              , ParseErrorType(..)
              , parse, unparse
              ) where

-- This is the Parser module, with the `parse` function which you'll
-- implement as <part 1> of the workshop. Its job is to convert strings
-- into data structures that the evaluator can understand.


-- Parse the string representation of one *single* expression into
-- the corresponding Abstract Syntax Tree.
parse :: String -> Parsed
parse source =
  ParsedString "Implement this function!"




-- Below are a few useful utility functions that should come in handy when
-- implementing the `parse` function. We don't want to spend the day
-- implementing parenthesis counting, after all...


-- AST representation of the parsed expressions.
data Parsed = ParsedString String
            | ParsedBool Bool
            | ParsedInt Int
            | ParsedList [Parsed]
            | ParseError ParseErrorType
            deriving (Show, Eq)

-- AST representation of parser errors.
data ParseErrorType = IncompleteExpression
                    | ExpressionTooLarge
                    deriving (Show, Eq)


-- Remove from a string, anything between a semicolon (;)
-- and a linebreak (\n).
removeComments :: String -> String
removeComments source =
  unlines $ takeWhile (/=';') <$> lines source


-- Given a string and a start-index, tries to find the index
-- of the next closing parenthesis that appears in the string.
findClosingParen :: String -> Int -> Either ParseErrorType Int
findClosingParen source startIndex =
    let subExp = takeWhile (/= ')') . drop startIndex $ source
        index  = startIndex + length subExp
    in if index >= length source || source !! index /= ')'
       then Left IncompleteExpression
       else Right index


-- Tries to split the string into (exp, rest) where exp
-- is the first expression in the string, and rest is
-- the remainder of the string after this expression.
firstExpression :: String -> Either ParseErrorType (String, String)
firstExpression "" = Right ("", "")
firstExpression (' ':rest) = firstExpression rest
firstExpression ('(':rest) =
  let subExpFrom i = ('(' : take (i + 1) rest, drop (i + 1) rest)
  in  subExpFrom <$> findClosingParen rest 0
firstExpression exps = Right $ span (/= ' ') exps


-- Tries to splits the string into a list of sub-expressions
-- that can be parsed individually.
splitExpressions :: String -> Either ParseErrorType [String]
splitExpressions source =
  case firstExpression source of
    Left parsingError -> Left parsingError
    Right ("", _)     -> Right []
    Right (exp, rest) -> (exp :) <$> splitExpressions rest




-- The functions below: `parseMultiple` and `unparse` are
-- implemented in order for the REPL to work.
-- Don't worry about them when implementing the language.


-- Tries to create a list of ASTs from a program source
-- string consisting of multiple expressions.
parseMultiple :: String -> Parsed
parseMultiple source =
  case splitExpressions $ removeComments source of
    Left parsingError -> ParseError parsingError
    Right expressions -> ParsedList $ parse <$> expressions


-- Turns an AST back into the program source string.
unparse :: Parsed -> String
unparse (ParsedString string)             = string
unparse (ParsedBool True)                 = "#t"
unparse (ParsedBool False)                = "#f"
unparse (ParsedInt int)                   = show int
unparse (ParsedList list)                 = concatMap unparse list
unparse (ParseError IncompleteExpression) = "Error: Incomplete expression!"
unparse (ParseError ExpressionTooLarge)   = "Error: Expression too large!"
