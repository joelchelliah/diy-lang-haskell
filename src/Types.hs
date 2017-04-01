module Types where


-- This module holds the types which will represent our
-- abstract syntax tree, as well as the Environment type
-- which will keep track of stored values during evaluation.
--
-- You are not required to make any changes in this file as part of
-- the workshop.

-- The types representing the different parts of our AST.
data DiyAST = DiySymbol String
            | DiyBool Bool
            | DiyInt Int
            | DiyList [DiyAST]
            | DiyClosure { func     :: DiyFunction
                         , localEnv :: Environment
                         }
            | DiyError DiyErrorType
            deriving (Show, Eq)


-- The types representing all the possible errors which may
-- occur during the parsing or evaluation phases.
data DiyErrorType = IncompleteExpression
                  | ExpressionTooLarge
                  | InvalidArgument
                  -- Errors related the environment
                  | LookUpError String
                  -- Errors related to function calls
                  | NotAFunction
                  | InvalidFunctionArguments { expected :: Int
                                             , received :: Int
                                             }
                  | EmptyFunctionCall
                  -- Errors related to lists
                  | AccessingEmptyList
                  | NotAList
                 deriving (Show, Eq)


-- Represents the environment, holding a list of all
-- (key, value) bindings created during an evaluation.
newtype Environment = Environment { bindings :: [(String, DiyAST)]
                                  } deriving (Show, Eq)


-- The function types below will be useful when doing <part 5>.
-- Feel free to ignore them until then.


-- Represents the function held by the DiyClosure,
-- consistings of a parameter list and a function body.
data DiyFunction = DiyFunction { params :: DiyFunctionParams
                               , body   :: DiyFunctionBody
                               } deriving (Show, Eq)


-- Some helpful aliases for distinguishing the
--  closure's function params, and function body.
type DiyFunctionParams = [DiyAST]
type DiyFunctionBody = DiyAST
