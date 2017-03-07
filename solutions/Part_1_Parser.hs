module Part_1_Parser where

import           Data.Char  (isDigit)
import           Parser     hiding (parse)
import           ParserUtil
import           Types


-- Solution for part 1: Parser

parse :: String -> Parsed
parse source =
  case clean source of
    "#t"       -> ParsedBool True
    "#f"       -> ParsedBool False
    ('\'':exp) -> parseQuote exp
    ('(':rest) -> parseList $ init rest
    (')':_)    -> ParseError ExpressionTooLarge
    other      -> parseIntOrSymbol other

  where clean = removeWhiteSpace . removeComments

        removeWhiteSpace = unwords . filter ((0 <) . length) . lines

        parseIntOrSymbol src =
          if all isDigit src
          then ParsedInt $ read src
          else ParsedSymbol src

        parseQuote src = ParsedList [ ParsedSymbol "quote", parse src ]

        parseList src =
          case splitExpressions src of
            Right exps -> foldr listOrError (ParsedList []) $ parse <$> exps
            Left  oops -> ParseError IncompleteExpression

        listOrError (ParseError err) _    = ParseError err
        listOrError exp (ParsedList exps) = ParsedList (exp:exps)
        listOrError _ (ParseError err)    = ParseError err
