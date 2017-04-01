module ParserUtil where

-- A few useful utility functions that should come in handy when
-- implementing the `parse` function in Parser.hs. We don't want to
-- spend the day implementing parenthesis counting, after all.
--
-- You are not required to make any changes in this file as part of
-- the workshop.


-- Errors that can be produced by a utility function.
newtype UtilError = UtilError String


-- Remove from a string, anything between a semicolon (;)
-- and a linebreak (\n).
removeComments :: String -> String
removeComments source =
  unlines $ takeWhile (/=';') <$> lines source


-- Tries to splits the string into a list of sub-expressions
-- that can be parsed individually.
splitExpressions :: String -> Either UtilError [String]
splitExpressions source =
  case firstExpression source of
    Left utilityError -> Left utilityError
    Right ("", _)     -> Right []
    Right (exp, rest) -> (exp :) <$> splitExpressions rest


-- Tries to split the string into (exp, rest) where exp
-- is the first expression in the string, and rest is
-- the remainder of the string after this expression.
firstExpression :: String -> Either UtilError (String, String)
firstExpression src@('\'':_) =
    addQuotes <$> firstExpression rest
    where (quotes, rest)        = span (== '\'') src
          addQuotes (exp, exps) = (quotes ++ exp, exps)

firstExpression src@('(':_) =
  subExpFrom <$> findMatchingParen src 0
  where subExpFrom i = (take (i + 1) src, drop (i + 1) src)

firstExpression (' ':rest) = firstExpression rest
firstExpression ""         = Right ("", "")
firstExpression src        = Right $ span (/= ' ') src


-- Given a string and the index of an opening parenthesis, tries
-- to find the index of the matching closing parenthesis.
findMatchingParen :: String -> Int -> Either UtilError Int
findMatchingParen source startIndex =
  if startIndex >= length source || source !! startIndex /= '('
  then Left invalidIndex
  else find (drop searchIndex source) searchIndex 0

  where searchIndex               = startIndex + 1
        find (')':_) index 0      = Right index
        find (')':t) index pCount = find t (index + 1) (pCount - 1)
        find ('(':t) index pCount = find t (index + 1) (pCount + 1)
        find (h:t)   index pCount = find t (index + 1) pCount
        find ""      _     _      = Left missingParen
        invalidIndex = UtilError "Opening parenthesis not found at given index!"
        missingParen = UtilError "Matching parenthesis not found!"
