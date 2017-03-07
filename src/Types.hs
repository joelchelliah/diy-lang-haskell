module Types where


-- This module holds all the types we will be using to represent our
-- abstract syntax tree. There are still a few types missing here,
-- which you will be implementing in the later parts of the workshop.


-- The types representing the different parts of our AST.
data DiyAST = DiySymbol String
            | DiyBool Bool
            | DiyInt Int
            | DiyList [DiyAST]
            | DiyError DiyErrorType
            deriving (Show, Eq)

-- The types representing all the possible errors which may occur during
-- the parsing or evaluation phases.
data DiyErrorType = IncompleteExpression
                  | ExpressionTooLarge
                  | InvalidArgument
                 deriving (Show, Eq)


-- Implement this type in <part 4> :
-- data Environment = ???
