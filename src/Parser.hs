module Parser ( Parsed(..)
              , ParseErrorType(..)
              , parse
              , findMatchingParen
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


-- AST representation of the parsed expressions
data Parsed = ParsedString String
            | ParsedBool Bool
            | ParsedInt Int
            | ParsedList [Parsed]
            | ParseError ParseErrorType
            deriving (Show, Eq)

-- AST representation of parser errors
data ParseErrorType = IncompleteExpression
                    | ExpressionTooLarge
                    deriving (Show, Eq)


-- Remove from a string anything in between a ; and a linebreak
removeComments :: String -> String
removeComments source =
  unlines $ takeWhile (/=';') <$> lines source


-- Given a string and the index of an opening parenthesis, determines
-- the index of the matching closing parenthesis
findMatchingParen :: String -> Int -> Maybe Int
findMatchingParen source index =
  Nothing
