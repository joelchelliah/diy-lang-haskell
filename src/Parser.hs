module Parser ( Parsed(..)
              , ParseErrorType(..)
              , parse
              ) where

-- AST representations of the parsed input
data Parsed = ParsedString String
            | ParsedBool Bool
            | ParsedInt Int
            | ParsedList [Parsed]
            | ParseError ParseErrorType
            deriving (Show, Eq)

-- AST representations of parse errors
data ParseErrorType = IncompleteExpression
                    | ExpressionTooLarge
                    deriving (Show, Eq)


-- Parse input and generate AST
parse :: String -> Parsed
parse source = ParsedString "Yay output!"
