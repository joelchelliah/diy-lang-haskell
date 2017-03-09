module ParserSolution where

import           Data.Char  (isDigit)
import           ParserUtil
import           Types


-- Solution for part 1:

parse' :: String -> DiyAST
parse' source =
  case clean source of
    "#t"       -> DiyBool True
    "#f"       -> DiyBool False
    ('\'':exp) -> parseQuote exp
    ('(':rest) -> parseList $ init rest
    (')':_)    -> DiyError ExpressionTooLarge
    other      -> parseIntOrSymbol other

  where clean =
          removeWhiteSpace . removeComments

        removeWhiteSpace =
          unwords . concatMap words . filter ((0 <) . length) . lines

        parseIntOrSymbol src =
          if all isDigit src
          then DiyInt $ read src
          else DiySymbol src

        parseQuote src = DiyList [ DiySymbol "quote", parse' src ]

        parseList src =
          case splitExpressions src of
            Right exps -> foldr listOrError (DiyList []) $ parse' <$> exps
            Left  oops -> DiyError IncompleteExpression

        listOrError (DiyError err) _   = DiyError err
        listOrError exp (DiyList exps) = DiyList (exp:exps)
        listOrError _ (DiyError err)   = DiyError err
