module Types where


-- This module holds the types which will represent our
-- abstract syntax tree, as well as the Environment type
-- which will keep track of stored values during evaluation.


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
                  | LookUpError String
                 deriving (Show, Eq)


-- Represents the environment, holding a list of all
-- (key, value) bindings created during an evaluation.
newtype Environment = Environment { bindings :: [(String, DiyAST)]
                                  } deriving (Show, Eq)


-- Represents the function held by the DiyClosure,
-- consistings of a parameter list and a function body.
data DiyFunction = DiyFunction { params :: [DiyAST]
                               , body   :: DiyAST
                               } deriving (Show, Eq)
