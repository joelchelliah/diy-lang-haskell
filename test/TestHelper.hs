module TestHelper where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.FailFast

import           Environment                     (extend, lookup)
import           Evaluator                       (evaluate)
import           Interpreter                     (interpret, interpretFile)
import           Parser                          (parse)
import           Prelude                         hiding (lookup)
import           Types

--
-- This module contains some common helper functions that
-- are used to generate the different test cases.
--


-- Assertions for validating the result of parsing an input string.

assertParse :: (DiyAST, String) -> Assertion
assertParse (expected, input) =
  assertEqual (descParse input) expected $ parse input

descParse :: String -> String
descParse input = "parse \"" ++ input ++ "\""

-- Assertions for validating the result of evaluating an AST.
-- Both with and without an already existing environment.

assertEvaluateWithEnvironment :: Environment -> (DiyAST, DiyAST) -> Assertion
assertEvaluateWithEnvironment env (expected, input) =
  assertEqual description expected result

  where description = descEvaluate input env
        (result, _) = evaluate input env

assertEvaluateWithoutEnvironment :: (DiyAST, DiyAST) -> Assertion
assertEvaluateWithoutEnvironment =
  assertEvaluateWithEnvironment $ Environment []

descEvaluate :: DiyAST -> Environment -> String
descEvaluate input env =
  "evaluate (" ++ show input ++ ") (" ++ show env ++ ")"


-- Assertions for validating the environment.

assertLookUp :: Environment -> String -> DiyAST -> Assertion
assertLookUp env key expected =
  assertEqual ("lookup " ++ show env ++ " " ++ show key) expected result

  where result = lookup env key


-- Assertions for validating closures.

assertIsClosure :: DiyAST -> Assertion
assertIsClosure (DiyClosure _ _) = assertBool "is a closure" True
assertIsClosure exp              = assertFailure $ show exp ++ " is not a closure"

assertClosureFunction :: DiyAST -> DiyFunctionParams -> DiyFunctionBody -> Assertion
assertClosureFunction (DiyClosure func _) expectedParams expectedBody = do
  assertEqual "closure function params" expectedParams $ params func
  assertEqual "closure function body" expectedBody $ body func


-- Assertions for validating the result of interpreting an input string.
-- Both with and without an already existing environment.

assertInterpretWithEnvironment :: Environment -> (String, String) -> Assertion
assertInterpretWithEnvironment environment (expected, input) =
  assertEqual description expected result

  where description = "interpret " ++ show input
        (result, _) = interpret input environment

assertInterpretWithoutEnvironment :: (String, String) -> Assertion
assertInterpretWithoutEnvironment =
  assertInterpretWithEnvironment $ Environment []
