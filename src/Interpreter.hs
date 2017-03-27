module Interpreter where

import           Data.Char      (isDigit)
import           ParserSolution
import           ParserUtil
import           Types
import           Evaluator (evaluate)
import           Parser (parse, unparse, parseMultiple)


-- This is the Interpreter module, which can be used to interpret
-- DiyLang programs.


-- Interprets a DiyLang program from statement.
--
-- Given a program statement as string,
-- interprets the statement,
-- and returns the resulting DiyLang expression as a string.
interpret :: String -> Environment -> String
interpret source env =
  unparse result

  where (result, _) = evaluate (parse source) env


-- Interprets a DiyLang program from file.
--
-- Given the name of a DiyLang file,
-- interprets the series of statements within,
-- and returns the result of the last expression as a string.
interpretFile :: String -> IO ()
interpretFile fileName = do
  contents <- readFile fileName

  -- TODO: need to carry over the environment from each evaluated AST!
  let (result, _) = evaluate (parseMultiple contents) $ Environment []
  
  putStrLn $ "TODO! (file has " ++ show (length (lines contents)) ++ " lines)."
