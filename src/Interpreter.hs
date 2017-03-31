module Interpreter where

import           Data.Char      (isDigit)
import           Evaluator      (evaluate)
import           Parser         (parse, parseMultiple, unparse)
import           ParserSolution
import           ParserUtil
import           Types


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
interpretFile :: String -> IO Environment
interpretFile fileName = do
  content <- readFile fileName

  let buildEnv ast env = snd (evaluate ast env)
      asts     = parseMultiple content

  return $ foldr buildEnv (Environment []) asts

  --putStrLn $ "TODO! (file has " ++ show (length (lines content)) ++ " lines)."


testParsing :: String -> IO [DiyAST]
testParsing fileName = do
  content <- readFile fileName

  return $ parseMultiple content
