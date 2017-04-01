module Interpreter where

import           Data.Char       (isDigit)
import           Data.List       (foldl')
import           Evaluator       (evaluate)
import           InterpreterUtil (parseMultiple, unparse)
import           Parser          (parse)
import           Types


-- This is the Interpreter module, which can be used to interpret
-- DiyLang programs.
--
-- You are not required to make any changes in this file as part of
-- the workshop.


-- Interprets a DiyLang program from statement.
-- Given a program statement as string, and an environment,
-- it interprets the statement, and returns the result of the
-- expression as a string, along with the environment.
interpret :: String -> Environment -> String
interpret source env =
  unparse result

  where (result, _) = evaluate (parse source) env


-- Interprets a DiyLang program from file.
-- Given the name of a DiyLang file, interprets the series of
-- statements within, and returns the result of the last
-- expression as a string, along with the environment.
interpretFile :: String -> IO (String, Environment)
interpretFile fileName = do
  content <- readFile fileName

  let parsedAsts        = parseMultiple content
      eval (_, env) ast = evaluate ast env
      (ast', env')      = foldl' eval (DiyBool True, Environment []) parsedAsts

  return (unparse ast', env')
