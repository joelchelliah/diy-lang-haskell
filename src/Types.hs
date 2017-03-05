module Types where


-- This module holds all the types we will be using to represent our language.
-- There are still a few missing types here, which you will be implementing
-- in the later parts workshop.


-- The types representing the different parts of our AST.
data Parsed = ParsedSymbol String
            | ParsedBool Bool
            | ParsedInt Int
            | ParsedList [Parsed]
            | ParseError ParserErrorType
            deriving (Show, Eq)

-- The types representing all the possible errors which may occur during
-- the parsing or evaluation phases.
data ParserErrorType = IncompleteExpression
                     | ExpressionTooLarge
                     | InvalidArgument
                     deriving (Show, Eq)
